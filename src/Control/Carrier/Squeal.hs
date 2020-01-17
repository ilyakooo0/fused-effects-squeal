module Control.Carrier.Squeal
  ( DBConnection,
    runSquealWithConn,
    runSquealWithConnRethrow,
    runSquealWithConn',
    SquealC (..),
    getSquealPool,
    runSqueal',
    runSqueal,
    runSquealPool,
    runSquealRethrow,
    module Control.Algebra,
  )
where

import Control.Algebra
import Control.Carrier.Orphans ()
import Control.Effect.Squeal
import Control.Monad.IO.Unlift
import qualified Squeal.PostgreSQL as Sq
import UnliftIO
import UnliftIO.Pool

newtype SquealC schemas m k = SquealC {unSquealC :: DBConnection schemas -> m k}

instance Functor m => Functor (SquealC schemas m) where
  fmap f (SquealC mk) = SquealC $ (fmap . fmap) f mk
  {-# INLINE fmap #-}

instance Applicative m => Applicative (SquealC schemas m) where
  pure x = SquealC $ \_ -> pure x
  {-# INLINE pure #-}
  (SquealC mklhs) <*> (SquealC mkrhs) = SquealC $ \r -> mklhs r <*> mkrhs r
  {-# INLINE (<*>) #-}

instance Monad m => Monad (SquealC schemas m) where
  (SquealC mk) >>= f = SquealC $ \r -> mk r >>= (runSquealWithConn' r . f)
  {-# INLINE (>>=) #-}

instance MonadIO m => MonadIO (SquealC schemas m) where
  liftIO = SquealC . const . liftIO
  {-# INLINE liftIO #-}

instance MonadUnliftIO m => MonadUnliftIO (SquealC schemas m) where
  withRunInIO inner = SquealC $ \r -> withRunInIO (\f -> inner (f . runSquealWithConn' r))
  {-# INLINE withRunInIO #-}

instance (MonadUnliftIO m, Algebra sig m) => Algebra (Squeal schemas :+: sig) (SquealC schemas m) where
  alg (L (ManipulateParams man x mk)) = SquealC $ \r -> do
    res <- flip evalPQ r $ Sq.manipulateParams man x
    runSquealWithConn' r $ mk res
  alg (L (TraversePrepared man x mk)) = SquealC $ \r -> do
    res <- flip evalPQ r $ Sq.traversePrepared man x
    runSquealWithConn' r $ mk res
  alg (L (TraversePrepared_ man x mk)) = SquealC $ \r -> do
    flip evalPQ r $ Sq.traversePrepared_ man x
    runSquealWithConn' r mk
  alg (R other) = SquealC $ \r -> alg . hmap (runSquealWithConn' r) $ other
  {-# INLINE alg #-}

-- | Run a squeal session using the given database connection without a transaction and without any error handling. You probably shouldn't use this.
runSquealWithConn' :: DBConnection schemas -> SquealC schemas m k -> m k
runSquealWithConn' r (SquealC mk) = mk r
{-# INLINE runSquealWithConn' #-}

-- | Run a squeal session using the given database connection, transaction mode and error handler.
runSquealWithConn ::
  MonadUnliftIO m =>
  DBConnection schemas ->
  Maybe TransactionMode ->
  (SquealException -> m k) ->
  SquealC schemas m k ->
  m k
runSquealWithConn db tr er mk =
  handleSqueal er $ maybe id (transactionallyRetry' db) tr (runSquealWithConn' db mk)
  where
    transactionallyRetry' ::
      (MonadUnliftIO m) =>
      DBConnection schemas ->
      TransactionMode ->
      m x ->
      m x
    transactionallyRetry' conn mode tx = mask $ \restore ->
      loop . try $ do
        x <- restore tx
        flip evalPQ conn $ Sq.manipulate_ commit
        return x
      where
        loop attempt = do
          flip evalPQ conn $ Sq.manipulate_ $ begin mode
          attempt >>= \case
            Left (PQException (PQState _ (Just "40001") _)) -> do
              flip evalPQ conn $ Sq.manipulate_ rollback
              loop attempt
            Left err -> do
              flip evalPQ conn $ Sq.manipulate_ rollback
              throwIO err
            Right x -> return x
{-# INLINE runSquealWithConn #-}

-- | Run a squeal session using the given database connection and transaction mode. Errors will not be handled.
runSquealWithConnRethrow ::
  MonadUnliftIO m =>
  DBConnection schemas ->
  Maybe TransactionMode ->
  SquealC schemas m k ->
  m k
runSquealWithConnRethrow db tr = runSquealWithConn db tr throwIO
{-# INLINE runSquealWithConnRethrow #-}

newtype SquealPoolC schemas m k = SquealPoolC {unSquealPoolC :: Pool (DBConnection schemas) -> m k}

instance Functor m => Functor (SquealPoolC schemas m) where
  fmap f (SquealPoolC mk) = SquealPoolC $ (fmap . fmap) f mk
  {-# INLINE fmap #-}

instance Applicative m => Applicative (SquealPoolC schemas m) where
  pure x = SquealPoolC $ const $ pure x
  {-# INLINE pure #-}
  (SquealPoolC mklhs) <*> (SquealPoolC mkrhs) = SquealPoolC $ \r -> mklhs r <*> mkrhs r
  {-# INLINE (<*>) #-}

instance Monad m => Monad (SquealPoolC schemas m) where
  (SquealPoolC mk) >>= f = SquealPoolC $ \r -> mk r >>= (($ r) . unSquealPoolC . f)
  {-# INLINE (>>=) #-}

instance MonadIO m => MonadIO (SquealPoolC schemas m) where
  liftIO = SquealPoolC . const . liftIO
  {-# INLINE liftIO #-}

instance MonadUnliftIO m => MonadUnliftIO (SquealPoolC schemas m) where
  withRunInIO inner = SquealPoolC $ \r -> withRunInIO (\f -> inner (f . ($ r) . unSquealPoolC))
  {-# INLINE withRunInIO #-}

runSquealPool :: Pool (DBConnection schemas) -> SquealPoolC schemas m k -> m k
runSquealPool conn (SquealPoolC f) = f conn
{-# INLINE runSquealPool #-}

instance Algebra sig m => Algebra (SquealPool schemas :+: sig) (SquealPoolC schemas m) where
  alg (L (GetSquealPool mk)) = SquealPoolC $ \r -> runSquealPool r $ mk r
  alg (R other) = SquealPoolC $ \r -> alg . hmap (runSquealPool r) $ other
  {-# INLINE alg #-}

-- | Run a squeal session picking a database connection from the connection pool without a transaction and without any error handling. You probably shouldn't use this.
runSqueal' ::
  (MonadUnliftIO m, Has (SquealPool schemas) sig m) =>
  SquealC schemas m k ->
  m k
runSqueal' = runSqueal Nothing throwIO
{-# INLINE runSqueal' #-}

-- | Run a squeal session picking a database connection from the connection pool with the given transaction mode and error handler.
runSqueal ::
  (MonadUnliftIO m, Has (SquealPool schemas) sig m) =>
  Maybe TransactionMode ->
  (SquealException -> m k) ->
  SquealC schemas m k ->
  m k
runSqueal tr er mk = do
  pool <- getSquealPool
  withResource pool $ \db ->
    runSquealWithConn db tr er mk
{-# INLINE runSqueal #-}

-- | Run a squeal session picking a database connection from the connection pool with the given transaction mode. Errors will not be handled.
runSquealRethrow ::
  (MonadUnliftIO m, Has (SquealPool schemas) sig m) =>
  Maybe TransactionMode ->
  SquealC schemas m k ->
  m k
runSquealRethrow tr mk = do
  pool <- getSquealPool
  withResource pool $ \db ->
    runSquealWithConn db tr throwIO mk
{-# INLINE runSquealRethrow #-}

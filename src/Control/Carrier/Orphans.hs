{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Carrier.Orphans
  (
  )
where

import Control.Carrier.Interpret
import Control.Carrier.Lift
import Control.Carrier.Reader
import qualified Control.Carrier.Trace.Ignoring as TI
import qualified Control.Carrier.Trace.Printing as TP
import Control.Monad.IO.Unlift

instance MonadUnliftIO m => MonadUnliftIO (LiftC m) where
  askUnliftIO = LiftC $ withUnliftIO $ \u -> return (UnliftIO (unliftIO u . runM))
  {-# INLINE askUnliftIO #-}

  withRunInIO inner = LiftC $ withRunInIO $ \run' -> inner (run' . runM)
  {-# INLINE withRunInIO #-}

instance MonadUnliftIO m => MonadUnliftIO (ReaderC r m) where
  askUnliftIO = ReaderC $ \r -> withUnliftIO $ \u -> pure (UnliftIO (\(ReaderC x) -> unliftIO u (x r)))
  {-# INLINE askUnliftIO #-}

  withRunInIO inner = ReaderC $ \r -> withRunInIO $ \go -> inner (go . runReader r)
  {-# INLINE withRunInIO #-}

instance MonadUnliftIO m => MonadUnliftIO (InterpretC s sig m) where
  askUnliftIO = InterpretC $ withUnliftIO $ \u -> return (UnliftIO (\(InterpretC m) -> unliftIO u m))
  {-# INLINE askUnliftIO #-}
  withRunInIO inner = InterpretC $ withRunInIO $ \run' -> inner (\(InterpretC m) -> run' m)
  {-# INLINE withRunInIO #-}

instance MonadUnliftIO m => MonadUnliftIO (TI.TraceC m) where
  askUnliftIO = TI.TraceC $ withUnliftIO $ \u -> return (UnliftIO (unliftIO u . TI.runTrace))
  {-# INLINE askUnliftIO #-}
  withRunInIO inner = TI.TraceC $ withRunInIO $ \run' -> inner (run' . TI.runTrace)
  {-# INLINE withRunInIO #-}

instance MonadUnliftIO m => MonadUnliftIO (TP.TraceC m) where
  askUnliftIO = TP.TraceC $ withUnliftIO $ \u -> return (UnliftIO (unliftIO u . TP.runTrace))
  {-# INLINE askUnliftIO #-}
  withRunInIO inner = TP.TraceC $ withRunInIO $ \run' -> inner (run' . TP.runTrace)
  {-# INLINE withRunInIO #-}

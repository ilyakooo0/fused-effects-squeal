module Control.Effect.Squeal
  ( Squeal (..),
    manipulateParams,
    manipulateParams_,
    manipulate,
    manipulate_,
    runQueryParams,
    runQuery,
    traversePrepared,
    forPrepared,
    traversePrepared_,
    forPrepared_,

    -- * Pool
    DBConnection,
    SquealPool (..),
    getSquealPool,

    -- * Reexports
    module Sq,
    module Control.Algebra,
  )
where

import Control.Algebra
import Control.Carrier.Orphans ()
import Data.Functor
import Squeal.PostgreSQL as Sq hiding
  ( Has,
    forPrepared,
    forPrepared_,
    manipulate,
    manipulateParams,
    manipulateParams_,
    manipulate_,
    runQuery,
    runQueryParams,
    traversePrepared,
    traversePrepared_,
  )

type DBConnection (schemas :: SchemasType) = K Connection schemas

data Squeal (schemas :: SchemasType) m k where
  ManipulateParams ::
    Sq.ToParams x params =>
    Sq.Manipulation '[] schemas params ys ->
    x ->
    (Sq.K Sq.Result ys -> m k) ->
    Squeal schemas m k
  TraversePrepared ::
    (Sq.ToParams x params, Traversable list) =>
    Sq.Manipulation '[] schemas params ys ->
    list x ->
    (list (Sq.K Sq.Result ys) -> m k) ->
    Squeal schemas m k
  TraversePrepared_ ::
    (Sq.ToParams x params, Foldable list) =>
    Sq.Manipulation '[] schemas params '[] ->
    list x ->
    m k ->
    Squeal schemas m k

instance Functor m => Functor (Squeal schemas m) where
  fmap f (ManipulateParams man x mk) = ManipulateParams man x ((fmap . fmap) f mk)
  fmap f (TraversePrepared man x mk) = TraversePrepared man x ((fmap . fmap) f mk)
  fmap f (TraversePrepared_ man x mk) = TraversePrepared_ man x (fmap f mk)
  {-# INLINE fmap #-}

instance HFunctor (Squeal schemas) where
  hmap f (ManipulateParams man x mk) = ManipulateParams man x (fmap f mk)
  hmap f (TraversePrepared man x mk) = TraversePrepared man x (fmap f mk)
  hmap f (TraversePrepared_ man x mk) = TraversePrepared_ man x (f mk)
  {-# INLINE hmap #-}

instance Effect (Squeal schemas) where
  thread ctx f (ManipulateParams man x mk) = ManipulateParams man x $ \y -> f (ctx $> mk y)
  thread ctx f (TraversePrepared man x mk) = TraversePrepared man x $ \y -> f (ctx $> mk y)
  thread ctx f (TraversePrepared_ man x mk) = TraversePrepared_ man x $ f (ctx $> mk)
  {-# INLINE thread #-}

-- | See 'Sq.manipulateParams' from @squeal-postgresql@.
manipulateParams ::
  (Has (Squeal schemas) sig m, Sq.ToParams x params) =>
  Sq.Manipulation '[] schemas params ys ->
  x ->
  m (Sq.K Sq.Result ys)
manipulateParams man x = send $ ManipulateParams man x pure

-- | See 'Sq.manipulateParams_' from @squeal-postgresql@.
manipulateParams_ ::
  (Has (Squeal schemas) sig m, Sq.ToParams x params) =>
  Sq.Manipulation '[] schemas params ys ->
  x ->
  m ()
manipulateParams_ man x = manipulateParams man x $> ()

-- | See 'Sq.manipulate' from @squeal-postgresql@.
manipulate ::
  Has (Squeal schemas) sig m =>
  Sq.Manipulation '[] schemas '[] ys ->
  m (Sq.K Sq.Result ys)
manipulate man = manipulateParams man ()

-- | See 'Sq.manipulate_' from @squeal-postgresql@.
manipulate_ ::
  Has (Squeal schemas) sig m =>
  Sq.Manipulation '[] schemas '[] ys ->
  m ()
manipulate_ man = manipulate_ man $> ()

-- | See 'Sq.runQueryParams' from @squeal-postgresql@.
runQueryParams ::
  (Has (Squeal schemas) sig m, Sq.ToParams x params) =>
  Sq.Query '[] '[] schemas params ys ->
  x ->
  m (Sq.K Sq.Result ys)
runQueryParams = manipulateParams . Sq.queryStatement

-- | See 'Sq.runQuery' from @squeal-postgresql@.
runQuery ::
  Has (Squeal schemas) sig m =>
  Sq.Query '[] '[] schemas '[] ys ->
  m (Sq.K Sq.Result ys)
runQuery q = runQueryParams q ()

-- | See 'Sq.traversePrepared' from @squeal-postgresql@.
traversePrepared ::
  (Sq.ToParams x params, Traversable list, Has (Squeal schemas) sig m) =>
  Sq.Manipulation '[] schemas params ys ->
  list x ->
  m (list (Sq.K Sq.Result ys))
traversePrepared man l = send $ TraversePrepared man l pure

-- | See 'Sq.forPrepared' from @squeal-postgresql@.
forPrepared ::
  (Sq.ToParams x params, Traversable list, Has (Squeal schemas) sig m) =>
  list x ->
  Sq.Manipulation '[] schemas params ys ->
  m (list (Sq.K Sq.Result ys))
forPrepared = flip traversePrepared

-- | See 'Sq.traversePrepared_' from @squeal-postgresql@.
traversePrepared_ ::
  (Sq.ToParams x params, Foldable list, Has (Squeal schemas) sig m) =>
  Sq.Manipulation '[] schemas params '[] ->
  list x ->
  m ()
traversePrepared_ man l = send $ TraversePrepared_ man l (pure ())

-- | See 'Sq.forPrepared_' from @squeal-postgresql@.
forPrepared_ ::
  (Sq.ToParams x params, Foldable list, Has (Squeal schemas) sig m) =>
  list x ->
  Sq.Manipulation '[] schemas params '[] ->
  m ()
forPrepared_ = flip traversePrepared_

newtype SquealPool schemas m k = GetSquealPool (Pool (DBConnection schemas) -> m k)

instance Functor m => Functor (SquealPool schemas m) where
  fmap f (GetSquealPool mk) = GetSquealPool ((fmap . fmap) f mk)
  {-# INLINE fmap #-}

instance HFunctor (SquealPool schemas) where
  hmap f (GetSquealPool mk) = GetSquealPool (fmap f mk)
  {-# INLINE hmap #-}

instance Effect (SquealPool schemas) where
  thread ctx f (GetSquealPool mk) = GetSquealPool $ \y -> f (ctx $> mk y)
  {-# INLINE thread #-}

getSquealPool :: Has (SquealPool schemas) sig m => m (Pool (DBConnection schemas))
getSquealPool = send $ GetSquealPool pure

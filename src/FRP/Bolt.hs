module FRP.Bolt where

import Control.Category
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Fix

import Data.Monoid
import Data.Profunctor
import Data.Time.Clock

import Prelude hiding (id, (.))

newtype Bolt e m a b =
  Bolt {
    stepBolt :: NominalDiffTime -> a -> m (Either e b, Bolt e m a b)
    }

instance (Monad m) => Category (Bolt e m) where
  id = Bolt $ \_ a -> return (Right a, id)
  Bolt bc . Bolt ab = Bolt $ \dt a -> do
    (be, ab') <- ab dt a
    case be of
      Left e -> return (Left e, Bolt bc . ab')
      Right b -> do
        (ce, bc') <- bc dt b
        return (ce, bc' . ab')

instance Monad m => Arrow (Bolt e m) where
  arr f = Bolt $ \_ a -> return (Right $ f a, arr f)
  first (Bolt bc) = Bolt $ \dt (b, d) -> do
    (r, n) <- bc dt b
    return (flip (,) d <$> r, first n)

instance (Monoid e, Monad m) => ArrowZero (Bolt e m) where
  zeroArrow = Bolt $ \_ _ -> return (Left mempty, zeroArrow)

instance (Monoid e, Monad m) => ArrowPlus (Bolt e m) where
  Bolt xr <+> Bolt yr = Bolt $ \dt a -> do
    (xe, xr') <- xr dt a
    case xe of
      Right x -> return (Right x, xr' <+> Bolt yr)
      Left e1 -> do
        (ye, yr') <- yr dt a
        case ye of
          Right y -> return (Right y, xr' <+> yr')
          Left e2 -> return (Left $ mappend e1 e2, xr' <+> yr')

instance Monad m => ArrowChoice (Bolt e m) where
  left (Bolt bc) = Bolt $ \dt bd -> case bd of
    Left b -> do
      (ce, bc') <- bc dt b
      return (fmap Left ce, left bc')
    Right d -> return (Right (Right d), left $ Bolt bc)

instance MonadFix m => ArrowLoop (Bolt e m) where
  loop (Bolt bdcd) = Bolt $ \dt b -> do
    ((c, _), bdcd') <- mfix $ \ ~((_, d), _) -> 
      first ((\_ -> error "Feedback loop broken by inhibition") ||| id) 
      `liftM` 
      bdcd dt (b, d)
    return (Right c, loop bdcd')

instance Functor m => Profunctor (Bolt e m) where
  lmap ab (Bolt bc) = Bolt $ \dt b -> fmap (lmap ab) <$> bc dt (ab b)
  rmap bc (Bolt ab) = Bolt $ \dt a -> (right bc *** rmap bc) <$> ab dt a

instance Functor m => Strong (Bolt e m) where
  second' (Bolt bc) = Bolt $ \dt (d, b) -> (right ((,) d) *** second') <$> bc dt b

instance Applicative m => Choice (Bolt e m) where
  left' (Bolt bc) = Bolt $ \dt bd -> case bd of
    Left b -> (right Left *** left') <$> bc dt b
    Right d -> pure (Right (Right d), left' $ Bolt bc)

instance Functor m => Functor (Bolt e m a) where
  fmap f (Bolt b) = Bolt $ \dt a -> (\(r, n) -> (fmap f r, fmap f n)) <$> b dt a

instance Applicative m => Applicative (Bolt e m a) where
  pure a = Bolt $ \_ _ -> pure (Right a, pure a)
  Bolt fs <*> Bolt xs = Bolt $ \dt a -> com <$> fs dt a <*> xs dt a
    where com (a, b) (c, d) = (a <*> c, b <*> d)

instance Alternative m => Alternative (Bolt e m a) where
  empty = Bolt $ \_ _ -> empty
  Bolt x <|> Bolt y = Bolt $ \dt a -> x dt a <|> y dt a

accum :: Applicative m => (b -> a -> b) -> b -> Bolt e m a b
accum acc d = Bolt $ \_ a -> pure (Right d, accum acc $ acc d a)
                     
accumT :: Applicative m => (NominalDiffTime -> b -> a -> b) -> b -> Bolt e m a b
accumT acc d = Bolt $ \dt a -> pure (Right d, accumT acc $ acc dt d a)

accum1 :: Applicative m => (b -> a -> b) -> b -> Bolt e m a b
accum1 acc d = Bolt $ \_ a -> let n = acc d a in pure (Right n, accum1 acc n)

accumT1 :: Applicative m => (NominalDiffTime -> b -> a -> b) -> b -> Bolt e m a b
accumT1 acc d = Bolt $ \dt a -> let n = acc dt d a in pure (Right n, accumT1 acc n)

iterateW :: Applicative m => (b -> b) -> b -> Bolt e m a b
iterateW f a = Bolt $ \_ _ -> pure (Right a, iterateW f $ f a)

iterateTW :: Applicative m => (NominalDiffTime -> b -> b) -> b -> Bolt e m a b
iterateTW f a = Bolt $ \dt _ -> pure (Right a, iterateTW f $ f dt a)

unfold :: Applicative m => (s -> a -> (b, s)) -> s -> Bolt e m a b
unfold f seed = Bolt $ \_ a -> pure $ Right *** unfold f $ f seed a
  
unfoldT :: Applicative m => (NominalDiffTime -> s -> a -> (b, s)) -> s -> Bolt e m a b
unfoldT f seed = Bolt $ \dt a -> pure $ Right *** unfoldT f $ f dt seed a

enumFromW :: (Applicative m, Enum b) => b -> Bolt e m a b
enumFromW = iterateW succ

mconcatW :: (Applicative m, Monoid b) => Bolt e m b b
mconcatW = accum mappend mempty
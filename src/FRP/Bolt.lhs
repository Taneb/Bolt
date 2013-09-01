> module Bolt where

Bolt is an FRP library heavily inspired by (i.e., completely stolen from) netwire.

I am writing it mainly as a learning exercise, but also because I disagree with
a few of the decisions made in netwire (such as the Alternative and Profunctor
instances, as well as the decision to have a separate pure wire constructor),
and I would like to see how those decisions were reached.

The name "Bolt" is just a joke, because crossbow bolts are kind of like arrows.
Also, it conveys an idea of speed. Maybe that will become appropriate later; I
haven't yet benchmarked it whatsoever.

> import Control.Applicative
> import Control.Arrow
> import Control.Category
> import Control.Monad
> import Control.Monad.Fix

> import Data.Monoid
> import Data.Profunctor
> import Data.String
> import Data.Time.Clock

I use Data.Time.Clock.NominalDiffTime where netwire just uses a type synonym for
Double because NominalDiffTime represents exactly what I am using it for.

> import Prelude hiding (id, (.))

Using the version of these from Control.Category. I'd love for Category to be in
the Prelude, but I don't know if that will ever happen. Then again, it may be
one of the things going into GHC 7.8/7.10, which would be nice.

> newtype Bolt e m a b = 
>   Bolt {stepBolt :: NominalDiffTime -> a -> m (Either e b, Bolt e m a b)}

This type is basically the same as the equivalent in netwire (although I am not
planning to have a separate "pure" constructor like WPure. I really don't see
the point. To compose a wire you need it to be monadic, any way.)

> instance Monad m => Category (Bolt e m) where

This I think is the most obvious instance. Arrow-like FRP is useless if it isn't
compositional, after all.

>   id = Bolt $ \_ a -> return (Right a, id)

id does what you expect, just echoing the input, never failing.

>   Bolt bc . Bolt ab = Bolt $ \dt a -> do
>     (eb, ab') <- ab dt a

eb is the (possibly failed) intermediate value of type b, ab' is the next Bolt
from a to b

>     case eb of
>       Left e -> return (Left e, Bolt (bc . (+) dt) . ab')

We've got no choice but to propogate the error. We also increase the time delta
so that the left-hand bolt doesn't lose time because it was unevaluated.

>       Right b -> do
>         (ec, bc') <- bc dt b

Runaway indentation here, any suggestions on how to reduce it will be most
welcome. I'm thinking EitherT, possibly? Or would that not work? ...

>         return (ec, bc' . ab')

> instance Monad m => Arrow (Bolt e m) where

This is the next obvious instance. This style of FRP (although not FRP as a
whole) is centered around Arrows.

>   arr f = Bolt $ \_ a -> return (Right $ f a, arr f)

arr just repeatedly applies the function.

>   first (Bolt bc) = Bolt $ \dt (b, d) -> do
>     (ec, bc') <- bc dt b
>     return (flip (,) d <$> ec, first bc')

first does what you'd expect, too.

Now for the rest of the Arrow classes!

> instance Monad m => ArrowChoice (Bolt e m) where
>   left (Bolt bc) = Bolt $ \dt bord -> case bord of
>     Left b -> do
>       (ce, bc') <- bc dt b
>       return (Left <$> ce, left bc')

This do notation isn't strictly necessary, it just is a lot more readable, I
think.

>     Right d -> return (Right $ Right d, left . Bolt $ bc . (+) dt)

> instance Monad m => ArrowApply (Bolt e m) where
>   app = Bolt $ \dt (Bolt bc, b) -> do
>     (ce, _) <- bc dt b
>     return (ce, app)

Use the ArrowApply instance with caution, I do not know if it obeys the laws!

> instance MonadFix m => ArrowLoop (Bolt e m) where
>   loop (Bolt bdcd) = Bolt $ \dt b -> do
>     ((c, _), bdcd') <- mfix $ \ ~((_, d), _) ->
>       first (error "Feedback loop broken by inhibition" ||| id) 
>       `liftM` bdcd dt (b, d)
>     return (Right c, loop bdcd')

I'm afraid I must confess that the MonadFix instance was largely plagiarized
from netwire. However, I've restyled it and may have introduced errors. If you
find an error, please tell me!

> instance (Monoid e, Monad m) => ArrowZero (Bolt e m) where
>   zeroArrow = Bolt $ \_ _ -> return (Left mempty, zeroArrow)

> instance (Monoid e, Monad m) => ArrowPlus (Bolt e m) where
>   Bolt xr <+> Bolt yr = Bolt $ \dt a -> do
>     (ex, xr') <- xr dt a
>     case ex of
>       Right x -> return (Right x, xr' <+> Bolt yr)
>       Left e1 -> do
>         (ey, yr') <- yr dt a
>         case ey of
>           Right y -> return (Right y, xr' <+> yr')
>           Left e2 -> return (Left $ mappend e1 e2, xr' <+> yr')

The ArrowZero and ArrowPlus instances catch errors! How cool is that?
Note that unlike netwire, Bolt's Alternative instance does NOT catch errors.

All Arrows are Profunctors, so a Profunctor instance is easy. We can weaken the
constraints a tad, though...

> instance Functor m => Profunctor (Bolt e m) where
>   lmap f (Bolt bc) = Bolt $ \dt a -> second (lmap f) <$> bc dt (f a)
>   rmap f (Bolt ab) = Bolt $ \dt a -> fmap (fmap f *** rmap f) $ ab dt a

Not sure if the unsafe combinators have much point, but it could wind up being
more efficient this way in some cases.

> instance Functor m => Strong (Bolt e m) where
>   second' (Bolt ab) = Bolt $ \dt (c, a) -> (right ((,) c) *** second') <$> ab dt a

> instance Applicative m => Choice (Bolt e m) where
>   left' (Bolt ab) = Bolt $ \dt ac -> case ac of
>     Left a -> (right Left *** left') <$> ab dt a
>     Right c -> pure (Right (Right c), left' . Bolt $ ab . (+) dt)

Now for classes that only care about the final argument.

> instance Functor m => Functor (Bolt e m a) where
>   fmap = rmap

Sneaky code re-use! I'll be able to re-use more code once AMP happens.
For example, I can use the Strong and Choice instances to implement Arrow and 
ArrowChoice better.

> instance Applicative m => Applicative (Bolt e m a) where
>   pure b = Bolt $ \_ _ -> pure (Right b, pure b)
>   Bolt af <*> Bolt ax = Bolt $ \dt a -> liftA2 com (af dt a) (ax dt a)
>     where com (a, b) (c, d) = (a <*> c, b <*> d)

> instance Alternative m => Alternative (Bolt e m a) where
>   empty = Bolt $ \_ _ -> empty
>   Bolt ab1 <|> Bolt ab2 = Bolt $ \dt a -> ab1 dt a <|> ab2 dt a

The Alternative instance for Bolt is VERY DIFFERENT FROM NETWIRE'S!
Whereas netwire uses Alternative for error-catching (which I have left in the
ArrowZero and ArrowPlus instances), Bolt's Alternative combines.

I'm not sure if I should copy netwire's weird instances like Num and Show, so
I'll use CPP to include them conditionally.
EDIT: I've decided to roll with the weird instances. They don't do any harm.

> instance (Applicative m, Num b) => Num (Bolt e m a b) where
>   (+) = liftA2 (+)
>   (-) = liftA2 (-)
>   (*) = liftA2 (*)
>   negate = fmap negate
>   abs = fmap abs
>   signum = fmap signum
>   fromInteger = pure . fromInteger

> instance (Applicative m, Fractional b) => Fractional (Bolt e m a b) where
>   (/) = liftA2 (/)
>   recip = fmap recip
>   fromRational = pure . fromRational

> instance (Applicative m, Floating b) => Floating (Bolt e m a b) where
>   pi = pure pi
>   exp = fmap exp
>   sqrt = fmap sqrt
>   log = fmap log
>   (**) = liftA2 (**)
>   logBase = liftA2 logBase
>   sin = fmap sin
>   tan = fmap tan
>   cos = fmap cos
>   asin = fmap asin
>   atan = fmap atan
>   acos = fmap acos
>   sinh = fmap sinh
>   tanh = fmap tanh
>   cosh = fmap cosh
>   asinh = fmap asinh
>   atanh = fmap atanh
>   acosh = fmap acosh

> instance (Applicative m, IsString b) => IsString (Bolt e m a b) where
>   fromString = pure . fromString

> instance (Applicative m, Read b) => Read (Bolt e m a b) where
>   readsPrec i s = first pure <$> readsPrec i s

> instance (Applicative m, Monoid b) => Monoid (Bolt e m a b) where
>   mempty = pure mempty
>   mappend = liftA2 mappend

TODO:

Prove ArrowApply and ArrowLoop instances obey the laws.


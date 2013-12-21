module Control.Monad.Ideal

%access public
%default total

data Ideal : (Type -> Type) -> Type -> Type where
  P : {f : Type -> Type} -> a -> Ideal f a
  I : {f : Type -> Type} -> (f a) -> Ideal f a

class Functor f => Mu' (f : Type -> Type) where
  mu' : f (Ideal f a) -> f a

instance Mu' f => Functor (Ideal f) where
  map g (P a) = P $ g a
  map g (I fa) = I $ map g fa

instance Mu' f => Applicative (Ideal f) where
  pure a = P a
  (P g) <$> x = map g x
  (I fg) <$> (P a) = I $ mu' $ map (\k => P $ k a) fg
  (I fg) <$> (I fa) = I $ mu' $ map (\k => I $ map k fa) fg


instance Mu' f => Monad (Ideal f) where
  (P a) >>= k = k a
  (I fa) >>= k = I $ mu' $ map k fa


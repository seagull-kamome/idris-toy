module Control.Functor

%access public
%default total

-- class Functor (f : Type -> Type) where
--  map : (a -> b) -> f a -> f b

instance Functor (\a => c -> a) where
  map f g = f . g

-- FIXME:
-- instance Functor (\a => (a, c)) where
--   map f (x,y) = (f x, y)

-- instance Functor (Either a) where
--   map _ (Left x) = Left x
--   map f (Right x) = Rignt $ f x



-- functor_law_1: map id == id
-- functor_law_2: map (f.g) == map f . map g

class CoFunctor (f : Type -> Type) where
  map' : f b -> f a -> (b -> a)



-- contravariant_law1: contramap id == id
-- contravariant_law2: contramap (g.f) = contramap f . contramap g
class Contravariant (f : Type -> Type) where
  contramap : (a -> b) -> f b -> f a

instance Contravariant (\b => b -> c) where
  contramap f g = g . f


class CoContravariant (f : Type -> Type) where
  contramap' : f a -> f b -> (b -> a)


class Bifunctor (f : Type -> Type -> Type) where
  bimap : (a -> b) -> (c -> d) -> f a c -> f b d

instance Bifunctor Either where
  bimap f _ (Left x) = Left $ f x
  bimap _ g (Right x) = Right $ g x




class CoBifunctor (f : Type -> Type -> Type) where
  bimap' : f b d -> f a c -> (d -> c) -> (b -> a)


class Profunctor (f : Type -> Type -> Type) where
  dimap : (a -> b) -> (c -> d) -> f b c -> f a d

instance Profunctor (\b => \c => b -> c) where
  dimap f g h = g . h . f

class CoProfunctor (f : Type -> Type -> Type) where
  dimap' : f a d -> f b c -> (d -> c) -> (b -> a)



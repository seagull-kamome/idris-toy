module Data.Hashable

%access public
%default total


class Hashable a where
  hash : a -> Nat

finHash : Hashable a => a -> Fin (S n)
finHash {n} x with (natToFin ((hash x) `modNat` (S n)) (S n))
  | Just m = m
  | Nothing = 0 -- impossible

instance Hashable Int where hash = cast . abs
instance Hashable Integer where hash = cast . abs
instance Hashable Char where hash = cast . abs . the Int . cast
instance Hashable Float where hash = cast .abs . the Int . cast
instance Hashable Nat where hash = id
instance Hashable a => Hashable (Vect n a) where
  hash = sum . map hash
instance (Hashable a, Foldable f) => Hashable (f a) where
  hash = foldr (\a => (+ hash a)) 0
instance Hashable () where hash _ = 0
instance (Hashable a,Hashable b) => Hashable (a,b) where hash (a,b) = hash a * hash b

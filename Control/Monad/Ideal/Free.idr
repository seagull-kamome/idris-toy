module Control.Monad.Ideal.Free

import Control.Monad.Ideal

%access public
%default total


Free : (Type -> Type) -> (Type -> Type) -- 大文字で始まるシンボルは前方参照解決してくれないっぽい？

data Liberty : (Type -> Type) -> Type -> Type where
  ToLiberty : {f : Type -> Type} -> {a : Type} -> (f (Free f a)) -> Liberty f a

Free f = Ideal $ Liberty f

free : Functor f => f (Free f a) -> Free f a
free f = I $ ToLiberty f

instance Functor f => Functor (Liberty f) where
  map g (ToLiberty fa) = ToLiberty $ map (?gg) fa

instance Functor f => Mu' (Liberty f) where
  mu' (ToLiberty fa) = ToLiberty $ map (?ff) fa


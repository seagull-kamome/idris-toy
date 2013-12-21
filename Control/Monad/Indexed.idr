module Control.Monad.Indexed

%access public
%default total


class IndexedFunctor (f : Type -> Type -> Type -> Type) where
  imap : (a -> b) -> f i i a -> f i i b

-- fがIndexedFunctorな時点で、fの種は確定すると思うのだが、明示しないと受け付けてくれない...
infixl 2 <:$>
class IndexedFunctor f => IndexedApplicative (f : Type -> Type -> Type -> Type) where
  ipure : a -> f i i a
  (<:$>) : f i i (a -> b) -> f i i a -> f i i b

infixl 2 :>>=
class IndexedApplicative f => IndexedMonad (f : Type -> Type -> Type -> Type) where
  (:>>=) : f j k x -> (x -> f j k a) -> f i k a

iflatten : IndexedMonad f => f i j (f i j a) -> f i j a
iflatten a = a :>>= id

ireturn : IndexedMonad f => a -> f i i a
ireturn = ipure


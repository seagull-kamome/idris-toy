module Logic.Basic

%access public
%default total

namespace and
  infix 5 /\
  (/\) : Type -> Type -> Type 
  (/\) a b = (a,b)

  associative_law : (a /\ b) /\ c -> a /\ (b /\ c)
  associative_law ((a, b), c) = (a, (b, c))

  commulative_law : a /\ b -> b /\ a
  commulative_law (a, b) = (b, a)

namespace or
  infix 5 \/
  (\/) : Type -> Type -> Type
  (\/) a b = Either a b

  associative_law : ((a \/ b) \/ c) -> (a \/ (b \/ c))
  associative_law (Right c) = Right (Right c)
  associative_law (Left (Left a)) = Left a
  associative_law (Left (Right b)) = Right (Left b)

  commulative_law : a \/ b -> b \/ a
  commulative_law x = either x Right Left


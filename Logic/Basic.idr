module Logic.Basic

%access public
%default total

namespace and
  infix 5 /\
  (/\) : Type -> Type -> Type
  (/\) a b = (a,b)

  associative_law_1 : (a /\ b) /\ c -> a /\ (b /\ c)
  associative_law_1 ((a, b), c) = (a, (b, c))

  associative_law_2 : a /\ (b /\ c) -> (a /\ b) /\ c
  associative_law_2 (a, (b, c)) = ((a, b), c)

  commulative_law : a /\ b -> b /\ a
  commulative_law (a, b) = (b, a)

  idempotence_and : a /\ a -> a
  idempotence_and (a,a) = a

namespace or
  infix 5 \/
  (\/) : Type -> Type -> Type
  (\/) a b = Either a b

  associative_law_1 : ((a \/ b) \/ c) -> (a \/ (b \/ c))
  associative_law_1 (Right c) = Right $ Right c
  associative_law_1 (Left (Left a)) = Left a
  associative_law_1 (Left (Right b)) = Right (Left b)

  associative_law_2 : (a \/ (b \/ c)) -> ((a \/ b) \/ c)
  associative_law_2 (Left a) = Left $ Left a
  associative_law_2 (Right (Left b)) = Left $ Right b
  associative_law_2 (Right (Right c)) = Right c

  commulative_law : a \/ b -> b \/ a
  commulative_law x = either x Right Left

  idempotence_or : a \/ a -> a
  idempotence_or x = either x id id

partition_law : a /\ (b \/ c) -> (a /\ b) \/ (a /\ c)
partition_law (a, (Left b)) = Left (a, b)
partition_law (a, (Right c)) = Right (a, c)

absorption_law_1 : (a /\ b) \/ a -> a
absorption_law_1 (Left (a,b)) = a
absorption_law_1 (Right a) = a

absorption_law_2 : (a \/ b) /\ a -> a
absorption_law_2 (_, a) = a




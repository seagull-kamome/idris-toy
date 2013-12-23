module Logic.Basic

%access public
%default total

infixl 2 <=>
infixl 2 <-->

data (<=>) : (a : Type) -> (b : Type) -> Type where
   (<-->) : (a -> b) -> (b -> a) -> a <=> b

-- 等値の対称性
eq_sym : (a <=> b) -> (b <=> a)
eq_sym (f <--> g) = (g <--> f)


-- 等値性から射を導出..ってか取り出してるだけ
--   TODO:この演算子使い辛くね？
infixl 5 ->>
infixl 5 <<-
(->>): (a <=> b) -> a -> b
(->>) (f <--> _) = f

(<<-) : (a <=> b) -> b -> a
(<<-) (_ <--> g) = g


-- 三段論法
syllogism : (a -> b) -> (b -> c) -> a -> c
syllogism f g = g . f

-- 三段同値
syllogism_eq : a <=> b -> b <=> c -> a <=> c
syllogism_eq (f <--> f') (g <--> g') = (g . f) <--> (f' . g')

-- トートロジー同値
tautology_eq : a <=> a
tautology_eq = id <--> id


namespace and
  infixl 5 /\
  (/\) : Type -> Type -> Type
  (/\) a b = (a,b)

  associative_law : (a /\ b) /\ c <=> a /\ (b /\ c)
  associative_law = (\((a,b),c) => (a,(b,c))) <--> (\(a,(b,c)) => ((a,b),c))

  commulative_law : a /\ b -> b /\ a
  commulative_law (a, b) = (b, a)

  idempotence_and : a /\ a -> a
  idempotence_and (a,a) = a

  -- assoc : (a /\ b) /\ c = a /\ (b /\ c)
  -- assoc = ?assoc

namespace or
  infixl 5 \/
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

  associative_law : ((a \/ b) \/ c) <=> (a \/ (b \/ c))
  associative_law = associative_law_1 <--> associative_law_2

  commulative_law : a \/ b -> b \/ a  -- TODO: 片方変換できれば自動的に等値が導けるケース
  commulative_law x = either x Right Left

  idempotence_or : a \/ a -> a
  idempotence_or x = either x id id

absorption_law_1 : (a /\ b) \/ a -> a
absorption_law_1 (Left (a,b)) = a
absorption_law_1 (Right a) = a

absorption_law_2 : (a \/ b) /\ a -> a
absorption_law_2 (_, a) = a

partition_law : a /\ (b \/ c) <=> (a /\ b) \/ (a /\ c)
partition_law = l <--> r
  where
     l (a, (Left b)) = Left (a, b)
     l (a, (Right c)) = Right (a, c)
     r (Left (a,b)) = (a, Left b)
     r (Right (a,c)) = (a, Right c)

partition_law_1 : (b \/ c) /\ a <=> (a /\ b) \/ (a /\ c)
--partition_law_1 = let (f <--> g) = partition_law
--                   in (f . and.commulative_law) <--> (and.commulative_law . g)

dm_nor : Not (a \/ b) <=> (Not a) /\ (Not b)
dm_nand : Not (a /\ b) <=> (Not a) \/ (Not b)



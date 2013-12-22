module Data.Map.AssocList import Data.Map

%access public
%default total

AListMap : Type -> Type -> Type
AListMap k v = List (k,v)

instance Map AListMap where
  empty = Nil
  fromList = id
  toList = id
  listKeys = map fst
  listValues = map snd

  fromVect = List.toList
  toVect xs = (length xs ** Vect.fromList xs)
  vectKeys xs = let xs' = map fst xs in (length xs' ** Vect.fromList xs')
  vectValues xs = let xs' = map snd xs in (length xs' ** Vect.fromList xs')

  grub k xs = map snd $ filter (\x => fst x == k) xs
  lookup k xs = map snd $ find (\x => fst x == k) xs

  insert = (::)
  erase k = filter (\x => fst x /= k)
  eraseBy f = filter (not . f)


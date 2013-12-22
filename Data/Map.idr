module Data.Map

import Control.Monad.State

%access public
%default total


namespace immutable
  class Map (m : Type -> Type -> Type) where
    empty : m k v

    fromList : List (k,v) -> m k v
    toList : m k v -> List (k,v)
    listKeys : m k v -> List k
    listValues : m k v -> List v

    fromVect : Vect n (k,v) -> m k v
    toVect : m k v -> (n ** Vect n (k,v))
    vectKeys : m k v -> (n ** Vect n (k))
    vectValues : m k v -> (n ** Vect n (v))

    grub : Eq k => k -> m k v -> List v
    lookup : Eq k => k -> m k v -> Maybe v

    insert : (k, v) -> m k v -> m k v
    erase : Eq k => k -> m k v -> m k v
    eraseBy : ((k,v) -> Bool) -> m k v -> m k v

  infix 2 !!
  (!!) : (Eq k, Map m) => m k v -> k -> List v
  (!!) = flip grub

  infix 2 !
  (!) : (Eq k, Map m)  => m k v -> k -> Maybe v
  (!) = flip lookup

  insertBy : Map m => (v -> k) -> v -> m k v -> m k v
  insertBy f v x = insert (f v, v) x


namespace mutable
  class Map (m : (Type -> Type) -> Type -> Type -> Type) where
    empty : f (m f k v)

    fromList : List (k,v) -> f (m f k v)
    toList : m f k v -> f (List (k,v))
    listKeys : m f k v -> f (List k)
    listValues : m f k v -> f (List v)

    fromVect : Vect n (k,v) -> f (m f k v)
    toVect : m f k v -> f (n ** Vect n (k,v))
    vectKeys : m f k v -> f (n ** Vect n k)
    vectValues : m f k v -> f (n ** Vect n v)

    grub : k -> m f k v -> f (List v)
    lookup : k -> m f k v -> f (Maybe v)

    insert : (k, v) -> m f k v -> f ()
    erase : k -> m f k v -> f ()
    eraseBy : ((k,v) -> Bool) -> m f k v -> f ()
    wipe : m f k v -> f ()


  infix 2 !!
  (!!) : mutable.Map m => m f k v -> k -> f (List v)
  (!!) = flip grub

  infix 2 !
  (!) : mutable.Map m  => m f k v -> k -> f (Maybe v)
  (!) = flip lookup

  insertBy : (Monad f, mutable.Map m) => (v -> f k) -> v -> m f k v -> f ()
  insertBy f v x = do { k <- f v; insert (k, v) x }




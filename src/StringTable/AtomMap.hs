module StringTable.AtomMap  (
            -- * Map type
              AtomMap(..), Key          -- instance Eq,Show

            -- * Operators
            , (!), (\\)

            -- * Query
            , null
            , size
            , member
            , notMember
	    , lookup
            , findWithDefault
            
            -- * Construction
            , empty
            , singleton

            -- ** Insertion
            , insert
            , insertWith, insertWithKey, insertLookupWithKey
            
            -- ** Delete\/Update
            , delete
            , adjust
            , adjustWithKey
            , update
            , updateWithKey
            , updateLookupWithKey
            , alter
  
            -- * Combine

            -- ** Union
            , union         
            , unionWith          
            , unionWithKey
            , unions
            , unionsWith

            -- ** Difference
            , difference
            , differenceWith
            , differenceWithKey
            
            -- ** Intersection
            , intersection           
            , intersectionWith
            , intersectionWithKey

            -- * Traversal
            -- ** Map
            , map
            , mapWithKey
            , mapAccum
            , mapAccumWithKey
            
            -- ** Fold
            , fold
            , foldWithKey

            -- * Conversion
            , elems
            , keys
  	    , keysSet
            , assocs
            
            -- ** Lists
            , toList
            , fromList
            , fromListWith
            , fromListWithKey

            -- ** Ordered lists
            , toAscList
            , fromAscList
            , fromAscListWith
            , fromAscListWithKey
            , fromDistinctAscList

            -- * Filter 
            , filter
            , filterWithKey
            , partition
            , partitionWithKey

            , mapMaybe
            , mapMaybeWithKey
            , mapEither
            , mapEitherWithKey

            , split         
            , splitLookup   

            -- * Submap
            , isSubmapOf, isSubmapOfBy
            , isProperSubmapOf, isProperSubmapOfBy
            
            -- * Min\/Max

            , maxView
            , minView
            -- , findMin   
            -- , findMax
            , deleteMin
            , deleteMax
            , deleteFindMin
            , deleteFindMax
            , updateMin
            , updateMax
            , updateMinWithKey
            , updateMaxWithKey 
            , minViewWithKey
            , maxViewWithKey

            -- * Debugging
            , showTree
            , showTreeWith
            ) where

import Prelude hiding (lookup, filter, map)
import qualified Prelude (lookup, filter, map)
import StringTable.Atom
import qualified Data.IntMap as IM
import qualified StringTable.AtomSet as AtomSet
import Data.Monoid
import Control.Monad (liftM)

type Key = Atom

newtype AtomMap a = MkAtomMap { fromAtomMap :: IM.IntMap a }
    deriving (Eq, Ord, Functor, Monoid)

instance Show a => Show (AtomMap a) where
    show = show . toList

fromList :: [(Atom, a)] -> AtomMap a
fromList ps = MkAtomMap $ IM.fromList [ (fromAtom l, x) | (l, x) <- ps ]

fromListWith :: (a -> a -> a) -> [(Atom, a)] -> AtomMap a
fromListWith f ps = MkAtomMap $ IM.fromListWith f [ (fromAtom l, x) | (l, x) <- ps ]

toList :: AtomMap a -> [(Atom, a)]
toList lm = [ (unsafeIntToAtom l, x) | (l, x) <- IM.toList $ fromAtomMap lm ]

map :: (a -> b) -> AtomMap a -> AtomMap b
map f = MkAtomMap . IM.map f . fromAtomMap

mapWithKey :: (Atom -> a -> b) -> AtomMap a -> AtomMap b
mapWithKey f lm = MkAtomMap $ IM.mapWithKey (f . unsafeIntToAtom) (fromAtomMap lm)

elems :: AtomMap a -> [a]
elems = IM.elems . fromAtomMap

union :: AtomMap a -> AtomMap a -> AtomMap a
union (MkAtomMap x) (MkAtomMap y) = MkAtomMap (IM.union x y)

unionWith :: (a -> a -> a) -> AtomMap a -> AtomMap a -> AtomMap a
unionWith f (MkAtomMap x) (MkAtomMap y) = MkAtomMap (IM.unionWith f x y)

unions :: [AtomMap a] -> AtomMap a
unions = MkAtomMap . IM.unions . Prelude.map fromAtomMap

unionsWith :: (a -> a -> a) -> [AtomMap a] -> AtomMap a
unionsWith f = MkAtomMap . IM.unionsWith f . Prelude.map fromAtomMap

lookup :: Atom -> AtomMap a -> Maybe a
lookup l = IM.lookup (fromAtom l) . fromAtomMap

insert :: Atom -> a -> AtomMap a -> AtomMap a
insert l v = MkAtomMap . IM.insert (fromAtom l) v . fromAtomMap

insertWith :: (a -> a -> a) -> Atom -> a -> AtomMap a -> AtomMap a
insertWith f l v = MkAtomMap . IM.insertWith f (fromAtom l) v . fromAtomMap

member :: Atom -> AtomMap a -> Bool
member l = IM.member (fromAtom l) . fromAtomMap

keys :: AtomMap a -> [Atom]
keys = Prelude.map unsafeIntToAtom . IM.keys . fromAtomMap

keysSet :: AtomMap a -> AtomSet.AtomSet
keysSet x = AtomSet.MkAtomSet (IM.keysSet (fromAtomMap x))

filter :: (a -> Bool) -> AtomMap a -> AtomMap a
filter f = MkAtomMap . IM.filter f . fromAtomMap

mapMaybe :: (a -> Maybe b) -> AtomMap a -> AtomMap b
mapMaybe f = MkAtomMap . IM.mapMaybe f . fromAtomMap

mapMaybeWithKey :: (Atom -> a -> Maybe b) -> AtomMap a -> AtomMap b
mapMaybeWithKey f = MkAtomMap . IM.mapMaybeWithKey (f . unsafeIntToAtom) . fromAtomMap

intersection :: AtomMap a -> AtomMap b -> AtomMap a
intersection (MkAtomMap x) (MkAtomMap y) = MkAtomMap (IM.intersection x y)

(!) :: AtomMap a -> Key -> a
m ! k = (IM.!) (fromAtomMap m) (fromAtom k)

(\\) :: AtomMap a -> AtomMap b -> AtomMap a
(\\) (MkAtomMap x) (MkAtomMap y) = MkAtomMap ((IM.\\) x y)

fromAscList :: [(Key,a)] -> AtomMap a
fromAscList xs = fromList xs

fromDistinctAscList :: [(Key,a)] -> AtomMap a
fromDistinctAscList xs = fromList xs

delete :: Key -> AtomMap a -> AtomMap a
delete x y = MkAtomMap (IM.delete (fromAtom x) (fromAtomMap y))

adjust ::  (a -> a) -> Key -> AtomMap a -> AtomMap a
adjust x y z = MkAtomMap (IM.adjust ( x) (fromAtom y) (fromAtomMap z))
adjustWithKey ::  (Key -> a -> a) -> Key -> AtomMap a -> AtomMap a
adjustWithKey x y z = MkAtomMap (IM.adjustWithKey ((. unsafeIntToAtom) x) (fromAtom y) (fromAtomMap z))
assocs :: AtomMap a -> [(Key,a)]
assocs x = Prelude.map (\(k, v) -> (unsafeIntToAtom k, v)) (IM.assocs (fromAtomMap x))
difference :: AtomMap a -> AtomMap b -> AtomMap a
difference x y = MkAtomMap (IM.difference (fromAtomMap x) (fromAtomMap y))
differenceWith :: (a -> b -> Maybe a) -> AtomMap a -> AtomMap b -> AtomMap a
differenceWith x y z = MkAtomMap (IM.differenceWith ( x) (fromAtomMap y) (fromAtomMap z))
differenceWithKey :: (Key -> a -> b -> Maybe a) -> AtomMap a -> AtomMap b -> AtomMap a
differenceWithKey x y z = MkAtomMap (IM.differenceWithKey ((. unsafeIntToAtom) x) (fromAtomMap y) (fromAtomMap z))
empty :: AtomMap a
empty  = MkAtomMap (IM.empty )
filterWithKey :: (Key -> a -> Bool) -> AtomMap a -> AtomMap a
filterWithKey x y = MkAtomMap (IM.filterWithKey ((. unsafeIntToAtom) x) (fromAtomMap y))
findWithDefault :: a -> Key -> AtomMap a -> a
findWithDefault x y z =  (IM.findWithDefault ( x) (fromAtom y) (fromAtomMap z))
fold :: (a -> b -> b) -> b -> AtomMap a -> b
fold x y z =  (IM.fold ( x) ( y) (fromAtomMap z))
foldWithKey :: (Key -> a -> b -> b) -> b -> AtomMap a -> b
foldWithKey x y z =  (IM.foldWithKey ((. unsafeIntToAtom) x) ( y) (fromAtomMap z))
fromAscListWith :: (a -> a -> a) -> [(Key,a)] -> AtomMap a
fromAscListWith x y = MkAtomMap (IM.fromAscListWith ( x) (Prelude.map (\(k, v) -> (fromAtom k, v)) y))
fromAscListWithKey :: (Key -> a -> a -> a) -> [(Key,a)] -> AtomMap a
fromAscListWithKey x y = MkAtomMap (IM.fromAscListWithKey ((. unsafeIntToAtom) x) (Prelude.map (\(k, v) -> (fromAtom k, v)) y))
fromListWithKey :: (Key -> a -> a -> a) -> [(Key,a)] -> AtomMap a
fromListWithKey x y = MkAtomMap (IM.fromListWithKey ((. unsafeIntToAtom) x) (Prelude.map (\(k, v) -> (fromAtom k, v)) y))
insertLookupWithKey :: (Key -> a -> a -> a) -> Key -> a -> AtomMap a -> (Maybe a, AtomMap a)
insertLookupWithKey x y z w = (\(x, y) -> (x, MkAtomMap y)) (IM.insertLookupWithKey ((. unsafeIntToAtom) x) (fromAtom y) ( z) (fromAtomMap w))
insertWithKey :: (Key -> a -> a -> a) -> Key -> a -> AtomMap a -> AtomMap a
insertWithKey x y z w = MkAtomMap (IM.insertWithKey ((. unsafeIntToAtom) x) (fromAtom y) ( z) (fromAtomMap w))
intersectionWith :: (a -> b -> a) -> AtomMap a -> AtomMap b -> AtomMap a
intersectionWith x y z = MkAtomMap (IM.intersectionWith ( x) (fromAtomMap y) (fromAtomMap z))
intersectionWithKey :: (Key -> a -> b -> a) -> AtomMap a -> AtomMap b -> AtomMap a
intersectionWithKey x y z = MkAtomMap (IM.intersectionWithKey ((. unsafeIntToAtom) x) (fromAtomMap y) (fromAtomMap z))
isProperSubmapOf :: Eq a => AtomMap a -> AtomMap a -> Bool
isProperSubmapOf x y =  (IM.isProperSubmapOf (fromAtomMap x) (fromAtomMap y))
isProperSubmapOfBy :: (a -> b -> Bool) -> AtomMap a -> AtomMap b -> Bool
isProperSubmapOfBy x y z =  (IM.isProperSubmapOfBy ( x) (fromAtomMap y) (fromAtomMap z))
isSubmapOf :: Eq a => AtomMap a -> AtomMap a -> Bool
isSubmapOf x y =  (IM.isSubmapOf (fromAtomMap x) (fromAtomMap y))
isSubmapOfBy :: (a -> b -> Bool) -> AtomMap a -> AtomMap b -> Bool
isSubmapOfBy x y z =  (IM.isSubmapOfBy ( x) (fromAtomMap y) (fromAtomMap z))
mapAccum :: (a -> b -> (a,c)) -> a -> AtomMap b -> (a,AtomMap c)
mapAccum x y z = (\(x, y) -> (x, MkAtomMap y)) (IM.mapAccum ( x) ( y) (fromAtomMap z))
mapAccumWithKey :: (a -> Key -> b -> (a,c)) -> a -> AtomMap b -> (a,AtomMap c)
mapAccumWithKey x y z = (\(x, y) -> (x, MkAtomMap y)) (IM.mapAccumWithKey ((\f x y -> f x (unsafeIntToAtom y)) x) ( y) (fromAtomMap z))
mapEither :: (a -> Either b c) -> AtomMap a -> (AtomMap b, AtomMap c)
mapEither x y = (\(x, y) -> (MkAtomMap x, MkAtomMap y)) (IM.mapEither ( x) (fromAtomMap y))
mapEitherWithKey :: (Key -> a -> Either b c) -> AtomMap a -> (AtomMap b, AtomMap c)
mapEitherWithKey x y = (\(x, y) -> (MkAtomMap x, MkAtomMap y)) (IM.mapEitherWithKey ((. unsafeIntToAtom) x) (fromAtomMap y))
maxViewWithKey :: (Monad m) => AtomMap a -> m ((Key, a), AtomMap a)
maxViewWithKey x = case IM.maxViewWithKey (fromAtomMap x) of
    Just ((x, y), z) -> return ((unsafeIntToAtom x, y), MkAtomMap z) 
    _ -> fail "No maxViewWithKey"
minViewWithKey :: (Monad m) => AtomMap a -> m ((Key, a), AtomMap a)
minViewWithKey x = case IM.minViewWithKey (fromAtomMap x) of
    Just ((x, y), z) -> return ((unsafeIntToAtom x, y), MkAtomMap z) 
    _ -> fail "No minViewWithKey"
notMember :: Key -> AtomMap a -> Bool
notMember x y =  (IM.notMember (fromAtom x) (fromAtomMap y))
partition :: (a -> Bool) -> AtomMap a -> (AtomMap a,AtomMap a)
partition x y = (\(x, y) -> (MkAtomMap x, MkAtomMap y)) (IM.partition ( x) (fromAtomMap y))
partitionWithKey :: (Key -> a -> Bool) -> AtomMap a -> (AtomMap a,AtomMap a)
partitionWithKey x y = (\(x, y) -> (MkAtomMap x, MkAtomMap y)) (IM.partitionWithKey ((. unsafeIntToAtom) x) (fromAtomMap y))
showTree :: Show a => AtomMap a -> String
showTree x =  (IM.showTree (fromAtomMap x))
showTreeWith :: Show a => Bool -> Bool -> AtomMap a -> String
showTreeWith x y z =  (IM.showTreeWith ( x) ( y) (fromAtomMap z))
singleton :: Key -> a -> AtomMap a
singleton x y = MkAtomMap (IM.singleton (fromAtom x) ( y))
size :: AtomMap a -> Int
size x =  (IM.size (fromAtomMap x))
split :: Key -> AtomMap a -> (AtomMap a,AtomMap a)
split x y = (\(x, y) -> (MkAtomMap x, MkAtomMap y)) (IM.split (fromAtom x) (fromAtomMap y))
splitLookup :: Key -> AtomMap a -> (AtomMap a,Maybe a,AtomMap a)
splitLookup x y = (\(x, y, z) -> (MkAtomMap x, y, MkAtomMap z)) (IM.splitLookup (fromAtom x) (fromAtomMap y))
toAscList :: AtomMap a -> [(Key,a)]
toAscList x = Prelude.map (\(k, v) -> (unsafeIntToAtom k, v)) (IM.toAscList (fromAtomMap x))
unionWithKey :: (Key -> a -> a -> a) -> AtomMap a -> AtomMap a -> AtomMap a
unionWithKey x y z = MkAtomMap (IM.unionWithKey ((. unsafeIntToAtom) x) (fromAtomMap y) (fromAtomMap z))
update ::  (a -> Maybe a) -> Key -> AtomMap a -> AtomMap a
update x y z = MkAtomMap (IM.update ( x) (fromAtom y) (fromAtomMap z))
updateLookupWithKey ::  (Key -> a -> Maybe a) -> Key -> AtomMap a -> (Maybe a,AtomMap a)
updateLookupWithKey x y z = (\(x, y) -> (x, MkAtomMap y)) (IM.updateLookupWithKey ((. unsafeIntToAtom) x) (fromAtom y) (fromAtomMap z))
updateMax :: (a -> Maybe a) -> AtomMap a -> AtomMap a
updateMax x y = MkAtomMap (IM.updateMax (x) (fromAtomMap y))
updateMaxWithKey :: (Key -> a -> Maybe a) -> AtomMap a -> AtomMap a
updateMaxWithKey x y = MkAtomMap (IM.updateMaxWithKey ((. unsafeIntToAtom) x) (fromAtomMap y))
updateMin :: (a -> Maybe a) -> AtomMap a -> AtomMap a
updateMin x y = MkAtomMap (IM.updateMin ( x) (fromAtomMap y))
updateMinWithKey :: (Key -> a -> Maybe a) -> AtomMap a -> AtomMap a
updateMinWithKey x y = MkAtomMap (IM.updateMinWithKey ((. unsafeIntToAtom) x) (fromAtomMap y))
updateWithKey ::  (Key -> a -> Maybe a) -> Key -> AtomMap a -> AtomMap a
updateWithKey x y z = MkAtomMap (IM.updateWithKey ((. unsafeIntToAtom) x) (fromAtom y) (fromAtomMap z))
alter :: (Maybe a -> Maybe a) -> Key -> AtomMap a -> AtomMap a
alter x y z = MkAtomMap (IM.alter ( x) (fromAtom y) (fromAtomMap z))
maxView :: (Monad m) => AtomMap a -> m (a, AtomMap a)
maxView x = case IM.maxView (fromAtomMap x) of
    Just (x, y) -> return (x, MkAtomMap y)
    _ -> fail "No maxView"
minView :: (Monad m) => AtomMap a -> m (a, AtomMap a)
minView x = case IM.minView (fromAtomMap x) of
    Just (x, y) -> return (x, MkAtomMap y)
    _ -> fail "No minView"
-- findMax :: AtomMap a -> a
-- findMax x =  (IM.findMax (fromAtomMap x))
-- findMin :: AtomMap a -> a
-- findMin x =  (IM.findMin (fromAtomMap x))
deleteMax :: AtomMap a -> AtomMap a
deleteMax x = MkAtomMap (IM.deleteMax (fromAtomMap x))
deleteMin :: AtomMap a -> AtomMap a
deleteMin x = MkAtomMap (IM.deleteMin (fromAtomMap x))
deleteFindMax :: AtomMap a -> ((IM.Key, a), AtomMap a)
deleteFindMax x = (\(x, y) -> (x, MkAtomMap y)) (IM.deleteFindMax (fromAtomMap x))
deleteFindMin :: AtomMap a -> ((IM.Key, a), AtomMap a)
deleteFindMin x = (\(x, y) -> (x, MkAtomMap y)) (IM.deleteFindMin (fromAtomMap x))

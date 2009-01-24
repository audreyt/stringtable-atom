module StringTable.AtomSet  (
            -- * Set type
              AtomSet(..)          -- instance Eq,Show

            -- * Operators
            , (\\)

            -- * Query
            , null
            , size
            , member
            , notMember
            , isSubsetOf
            , isProperSubsetOf
            
            -- * Construction
            , empty
            , singleton
            , insert
            , delete
            
            -- * Combine
            , union, unions
            , difference
            , intersection
            
            -- * Filter
            , filter
            , partition
            , split
            , splitMember

            -- * Min\/Max
            , findMin   
            , findMax
            , deleteMin
            , deleteMax
            , deleteFindMin
            , deleteFindMax
            , maxView
            , minView

            -- * Map
	    , map
	    , mapMonotonic

            -- * Fold
            , fold

            -- * Conversion
            -- ** List
            , elems
            , toList
            , fromList
            
            -- ** Ordered list
            , toAscList
            , fromAscList
            , fromDistinctAscList
                        
            -- * Debugging
            , showTree
            , showTreeWith
            ) where

import Prelude hiding (lookup, filter, null, map)
import qualified Prelude (map)
import StringTable.Atom
import qualified Data.IntSet as IS
import Data.Monoid
import Control.Monad (liftM)

newtype AtomSet = MkAtomSet { fromAtomSet :: IS.IntSet }
    deriving (Eq, Ord)

null :: AtomSet -> Bool
null x =  (IS.null (fromAtomSet x))
size :: AtomSet -> Atom
size x = unsafeIntToAtom (IS.size (fromAtomSet x))
member :: Atom -> AtomSet -> Bool
member x y =  (IS.member (fromAtom x) (fromAtomSet y))
notMember :: Atom -> AtomSet -> Bool
notMember x y =  (IS.notMember (fromAtom x) (fromAtomSet y))
isSubsetOf :: AtomSet -> AtomSet -> Bool
isSubsetOf x y =  (IS.isSubsetOf (fromAtomSet x) (fromAtomSet y))
isProperSubsetOf :: AtomSet -> AtomSet -> Bool
isProperSubsetOf x y =  (IS.isProperSubsetOf (fromAtomSet x) (fromAtomSet y))
empty :: AtomSet
empty  = MkAtomSet (IS.empty )
singleton :: Atom -> AtomSet
singleton x = MkAtomSet (IS.singleton (fromAtom x))
insert :: Atom -> AtomSet -> AtomSet
insert x y = MkAtomSet (IS.insert (fromAtom x) (fromAtomSet y))
delete :: Atom -> AtomSet -> AtomSet
delete x y = MkAtomSet (IS.delete (fromAtom x) (fromAtomSet y))
unions :: [AtomSet] -> AtomSet
unions x = MkAtomSet (IS.unions (Prelude.map fromAtomSet x))
difference :: AtomSet -> AtomSet -> AtomSet
difference x y = MkAtomSet (IS.difference (fromAtomSet x) (fromAtomSet y))
intersection :: AtomSet -> AtomSet -> AtomSet
intersection x y = MkAtomSet (IS.intersection (fromAtomSet x) (fromAtomSet y))
filter :: (Atom -> Bool) -> AtomSet -> AtomSet
filter x y = MkAtomSet (IS.filter ((. unsafeIntToAtom) x) (fromAtomSet y))
partition :: (Atom -> Bool) -> AtomSet -> (AtomSet,AtomSet)
partition x y = (\(x, y) -> (MkAtomSet x, MkAtomSet y)) (IS.partition ((. unsafeIntToAtom) x) (fromAtomSet y))
split :: Atom -> AtomSet -> (AtomSet,AtomSet)
split x y = (\(x, y) -> (MkAtomSet x, MkAtomSet y)) (IS.split (fromAtom x) (fromAtomSet y))
splitMember :: Atom -> AtomSet -> (AtomSet,Bool,AtomSet)
splitMember x y = (\(x, y, z) -> (MkAtomSet x, y, MkAtomSet z)) (IS.splitMember (fromAtom x) (fromAtomSet y))
findMin :: AtomSet -> Atom
findMin x = unsafeIntToAtom (IS.findMin (fromAtomSet x))
findMax :: AtomSet -> Atom
findMax x = unsafeIntToAtom (IS.findMax (fromAtomSet x))
deleteMin :: AtomSet -> AtomSet
deleteMin x = MkAtomSet (IS.deleteMin (fromAtomSet x))
deleteMax :: AtomSet -> AtomSet
deleteMax x = MkAtomSet (IS.deleteMax (fromAtomSet x))
deleteFindMin :: AtomSet -> (Atom, AtomSet)
deleteFindMin x = (\(x, y) -> (unsafeIntToAtom x, MkAtomSet y)) (IS.deleteFindMin (fromAtomSet x))
deleteFindMax :: AtomSet -> (Atom, AtomSet)
deleteFindMax x = (\(x, y) -> (unsafeIntToAtom x, MkAtomSet y)) (IS.deleteFindMax (fromAtomSet x))
maxView :: (Monad m) => AtomSet -> m (Atom, AtomSet)
maxView x = liftM (\(x, y) -> (unsafeIntToAtom x, MkAtomSet y)) (IS.maxView (fromAtomSet x))
minView :: (Monad m) => AtomSet -> m (Atom, AtomSet)
minView x = liftM (\(x, y) -> (unsafeIntToAtom x, MkAtomSet y)) (IS.minView (fromAtomSet x))
map :: (Atom->Atom) -> AtomSet -> AtomSet
map x y = MkAtomSet (IS.map ((\f -> fromAtom . f . unsafeIntToAtom) x) (fromAtomSet y))
mapMonotonic :: (Atom->Atom) -> AtomSet -> AtomSet
mapMonotonic = map
fold :: (Atom -> b -> b) -> b -> AtomSet -> b
fold x y z =  (IS.fold ((. unsafeIntToAtom) x) ( y) (fromAtomSet z))
elems :: AtomSet -> [Atom]
elems x = Prelude.map unsafeIntToAtom (IS.elems (fromAtomSet x))
toList :: AtomSet -> [Atom]
toList x = Prelude.map unsafeIntToAtom (IS.toList (fromAtomSet x))
fromList :: [Atom] -> AtomSet
fromList x = MkAtomSet (IS.fromList (Prelude.map fromAtom x))
toAscList :: AtomSet -> [Atom]
toAscList x = Prelude.map unsafeIntToAtom (IS.toAscList (fromAtomSet x))
fromAscList :: [Atom] -> AtomSet 
fromAscList x = MkAtomSet (IS.fromAscList (Prelude.map fromAtom x))
fromDistinctAscList :: [Atom] -> AtomSet
fromDistinctAscList x = MkAtomSet (IS.fromDistinctAscList (Prelude.map fromAtom x))
showTree :: AtomSet -> String
showTree x =  (IS.showTree (fromAtomSet x))
showTreeWith :: Bool -> Bool -> AtomSet -> String
showTreeWith x y z =  (IS.showTreeWith ( x) ( y) (fromAtomSet z))
union :: AtomSet -> AtomSet -> AtomSet
union x y = MkAtomSet (IS.union (fromAtomSet x) (fromAtomSet y))
(\\) :: AtomSet -> AtomSet -> AtomSet
(\\) x y = MkAtomSet (IS.union (fromAtomSet x) (fromAtomSet y))

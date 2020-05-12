{-# LANGUAGE TemplateHaskell #-}
module ZeroSet.AlternatingPath where

import           Control.Lens ((^.))
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Ext
import qualified Data.List as List
import           Data.List.Alternating
import qualified Data.List.NonEmpty as NonEmpty

--------------------------------------------------------------------------------

-- | Models a path from some vertex v, via a bunch of edges e, to
-- another vertex v.  Note that if the source and the target vertex
-- are the same, the path just consists of a singleton vertex v.
data FromTo v e = Singleton !v
                | FromTo !v !(NonEmpty.NonEmpty e) !v
                deriving (Show,Eq,Read)

instance Bifunctor FromTo where
  bimap = bimapDefault
instance Bifoldable FromTo where
  bifoldMap = bifoldMapDefault
instance Bitraversable FromTo where
  bitraverse f g = \case
    Singleton x   -> Singleton <$> f x
    FromTo s es t -> FromTo <$> f s <*> traverse g es <*> f t

--------------------------------------------------------------------------------

-- | Computes the (start vertex, the edge sequence crossed, target vertex) if it exists
-- (and otherwise just returns the single vertex in the path)
alternatingFromTo :: Alternating v e -> FromTo v e
alternatingFromTo = \case
  Alternating s [] -> Singleton s
  Alternating s xs -> FromTo s (NonEmpty.fromList $ map (^.core) xs) ((List.last xs)^.extra)

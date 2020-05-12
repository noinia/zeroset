module ZeroSet.Main where

import Data.Geometry.Properties
import Control.Lens
import Data.Data
import Data.Ext
import Data.Geometry.Ball
import Data.Geometry.Point
import Data.Geometry.Box
import Data.Geometry.Ellipse (ellipseToCircle)
import Data.Geometry.Ipe
import Data.Geometry.PolyLine
import Data.Geometry.ZeroSet
import Data.Maybe (mapMaybe)
import Data.RealNumber.Rational
import Data.Util
import Options.Applicative

-- import Debug.Trace
--------------------------------------------------------------------------------


type R = RealNumber 10
-- type R = Double

data Options = Options { inFile  :: FilePath
                       , outFile :: FilePath
                       } deriving (Show,Eq,Data)

options :: ParserInfo Options
options = info (helper <*> parser)
               (  progDesc "given an ipe file with two polygonal chains, treated as prefixes of a path, and a rectangle, computes an approximation of the bisector in that rectangle."
               <> header   "zeroset"
               )
  where
    parser = Options
          <$> strOption (help "Input file (in ipe7 xml format)"
                         <> short 'i'
                        )
          <*> strOption (help "Output File (in ipe7 xml format)"
                         <> short 'o'
                        )


mainWith (Options inFile outFile) = readSinglePageFile inFile >>= \case
    Left err -> print err
    Right p  -> runPage p outFile




-- data Geoms  = Polies (Two (PolyLine 2 () R))
--             | Disks  (Two (Disk () R))
--             deriving (Show,Eq)

data ReadInput = ReadInput (Two (Point 2 R :+ Double)) (Rectangle () R)
               deriving (Show,Eq)

-- traceBisector'         :: ( RealFrac r, Ord r
--                          , ToWeightedPoint g Double, ToWeightedPoint h Double
--                          , NumType g ~ r, NumType h ~ r
--                          , IpeWriteText r, Show r
--                          )
--                       => ZeroConfig r
--                       -> g -> h
--                       -> Rectangle c r
--                       -> Maybe (PolyLine 2 () r)
-- traceBisector' cfg g h = traceBisector' cfg (toWeightedPoint g) (toWeightedPoint h)




-- type

-- readInput page | traceShow page False = undefined
readInput      :: IpePage R -> Either String ReadInput
readInput page = (\ps r -> ReadInput ps r) <$> geoms <*> rect
  where
    rect   = case page^..content.traverse._IpePath.core._asRectangle of
               [rect'] -> Right rect'
               _       -> Left "no rectangle"

    geoms = case    (toWeightedPoint <$> readPoints page)
                 <> (toWeightedPoint <$> readPolylines page)
                 <> (toWeightedPoint <$> readDisks page)
            of
              [p,q] -> Right (Two p q)
              _     -> Left "Expects exactly 2 geometric objects. Supported are (points, disks, and polylines)"


readPoints :: IpePage R -> [Point 2 R]
readPoints = map (^.core) . readAll

readPolylines :: IpePage R -> [PolyLine 2 () R]
readPolylines = map (^.core) . readAll

readDisks :: IpePage R -> [Disk () R]
readDisks = mapMaybe (ellipseToDisk) . map (^.core) . readAll
  where
    ellipseToDisk = fmap (view $ from _DiskCircle) . ellipseToCircle




runPage         :: IpePage R -> FilePath -> IO ()
runPage page fp = case res of
                    Left e   -> print e
                    Right pl -> writeIpeFile fp $ singlePageFromContent [iO $ defIO pl]
  where
    res = do (ReadInput gs rect') <- readInput page
             let Two p q = gs
             withError "tracing failed" $ traceBisector defaultZeroConfig q p rect'


withError   :: e -> Maybe a -> Either e a
withError e = maybe (Left e) Right

-- runPage      :: IpePage R -> IO ()
-- runPage page =
--                  (x1:x2:_) ->



--                                        x <- computation p
--                                        lift writeFile $ singlePageFromContent xs





-- do p <- readSinglePageFile


-- type M a = Either Text ()

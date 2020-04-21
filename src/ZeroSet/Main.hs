module ZeroSet.Main where

import Data.Geometry.Properties
import Control.Lens
import Data.Data
import Data.Ext
import Data.Geometry.Ball
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





data Geoms  = Polies (Two (PolyLine 2 () R))
            | Disks  (Two (Disk () R))
            deriving (Show,Eq)

data ReadInput = ReadInput Geoms (Rectangle () R)
               deriving (Show,Eq)


-- type

-- readInput page | traceShow page False = undefined
readInput      :: IpePage R -> Either String ReadInput
readInput page = (\ps r -> ReadInput ps r) <$> geoms <*> rect
  where
    rect   = case page^..content.traverse._IpePath.core._asRectangle of
               [rect'] -> Right rect'
               _       -> Left "no rectangle"

    geoms = (Polies <$> readPolylines page) <|> (Disks <$> readDisks page)


readAll' :: (HasDefaultFromIpe g, r ~ NumType g)
         => IpePage r -> [g :+ IpeAttributes (DefaultFromIpe g) r]
readAll' = readAll . Identity

readPolylines      :: IpePage R -> Either String (Two (PolyLine 2 () R))
readPolylines page = case map (^.core) $ readAll' page of
                       [p1,p2] -> Right $ Two p1 p2
                       pls     -> Left $ "no 2 polylines " <> show pls

readDisks      :: IpePage R -> Either String (Two (Disk () R))
readDisks page =  case mapMaybe (ellipseToDisk) . map (^.core) $ readAll' page of
                      [c1,c2] -> Right $ Two c1 c2
                      pls     -> Left $ "no 2 disks " <> show pls
  where
    ellipseToDisk = fmap (view $ from _DiskCircle) . ellipseToCircle

-- newtype IOE e a = IOE { runIOE :: IO (Either e a) }

-- instance Monad (IOE e) where
--   (IOE m) >>= k = IOE $ do e <- m
--                            case e of
--                              Left err -> pure $ Left err
--                              Right a  -> runIOE $ k a

-- runPage' :: IpePage r -> FilePath -> IO (Either e ())


runPage         :: IpePage R -> FilePath -> IO ()
runPage page fp = case res of
                    Left e   -> print e
                    Right pl -> writeIpeFile fp $ singlePageFromContent [iO $ defIO pl]
  where
    res = do (ReadInput gs rect) <- readInput page
             withError "tracing failed" $ case gs of
               Polies (Two p1 p2) -> traceFromPrefix    defaultZeroConfig p1 p2 rect
               Disks  (Two d1 d2) -> traceBisectorDisks defaultZeroConfig d1 d2 rect


withError   :: e -> Maybe a -> Either e a
withError e = maybe (Left e) Right

-- runPage      :: IpePage R -> IO ()
-- runPage page =
--                  (x1:x2:_) ->



--                                        x <- computation p
--                                        lift writeFile $ singlePageFromContent xs





-- do p <- readSinglePageFile


-- type M a = Either Text ()

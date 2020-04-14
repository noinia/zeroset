module ZeroSet.Main where

import Control.Lens
import Data.Data
import Data.Ext
import Data.Geometry.Box
import Data.Geometry.Ipe
import Data.Geometry.PolyLine
import Data.Geometry.ZeroSet
import Options.Applicative
-- import Data.RealNumber.Rational

import Debug.Trace
--------------------------------------------------------------------------------


-- type R = RealNumber 10

type R = Double

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


data ReadInput = ReadInput (PolyLine 2 () R)
                           (PolyLine 2 () R)
                           (Rectangle () R)
               deriving (Show,Eq)


-- type

readInput page | traceShow page False = undefined
readInput page = (\(p1,p2) r -> ReadInput p1 p2 r) <$> polies <*> rect
  where
    rect   = case page^..content.traverse._IpePath.core._asRectangle of
               [rect'] -> Right rect'
               _       -> Left "no rectangle"
    polies = case page^..content.traverse._IpePath.core._asPolyLine of
               [p1,p2] -> Right (p1,p2)
               pls     -> Left $ "no 2 polylines " <> show pls



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
    res = do (ReadInput p1 p2 rect) <- readInput page
             withError "tracing failed" $ traceFromPrefix defaultZeroConfig p1 p2 rect


withError   :: e -> Maybe a -> Either e a
withError e = maybe (Left e) Right

-- runPage      :: IpePage R -> IO ()
-- runPage page =
--                  (x1:x2:_) ->



--                                        x <- computation p
--                                        lift writeFile $ singlePageFromContent xs





-- do p <- readSinglePageFile


-- type M a = Either Text ()

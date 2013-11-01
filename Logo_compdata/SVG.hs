-- | This module contains the underlying type representing SVG images. 
-- For now, we only define an SVG as a single path, and the Show instance
-- outputs the SVG string for that single path, along with svg tags.
module SVG where

-- | Our representation of SVG images is a single path of /Step/s
data SVG = Path [Step]

-- | A /Position/ is simply a pair of coordinates
type Position = (Double,Double)

-- | A /Step/ is either a Move to a new position, or a Line to a new position.
data Step = M Position
          | L Position
  deriving (Eq,Show)

-- | The Show instance for /SVG/ outputs the svg tags, and a single path tag.
-- For now, the path starts at position (200,200).
instance Show SVG where
 show (Path ps) =   "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">\n"
                 ++ "<path d=\"M200 200" ++ show' ps ++ "Z\"  fill=\"none\" stroke=\"red\"/>\n"
                 ++ "</svg>" 

show' :: [Step] -> String
show' [] = ""
show' (M (x,y):ss) = "M" ++ show x ++ " " ++ show y ++ " " ++ show' ss
show' (L (x,y):ss) = "L" ++ show x ++ " " ++ show y ++ " " ++ show' ss
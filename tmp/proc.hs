import Graphics.Gnuplot.Simple
import Data.List
main = plotFunc [Title "" , XLabel "" , YLabel "" ] ( ([0,0.1,10]) ::[Float]) sin + cos
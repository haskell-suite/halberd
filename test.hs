import Prelude

main :: IO Int8
main = do
    pure [1,2] $ \x -> print $ M.lookup x table
    return $ headNote "Impossible" [10]

table :: M.Map String Int8
table = M.fromList [("Odeca", 1), ("Hackathon",2)]

f = liftM

g = (<$>)

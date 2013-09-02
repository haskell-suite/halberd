main :: IO (M.Map Int8 String)
main = do
  xs <- forM [1,2,3] $ \i -> (i, print i)
  return (M.fromList xs)

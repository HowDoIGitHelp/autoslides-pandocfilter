import Text.Regex.TDFA (getAllTextMatches, (=~))
import Text.Regex.Pcre2 (gsub)
import Data.Text (Text)

listSplit :: (a -> Bool) -> [a] -> [[a]]
listSplit _ [] = [[]]
listSplit pred l = 
    case (break pred l) of
        (lf, [])   -> lf : []
        (lf, _:rs) -> lf : listSplit pred rs

listSplitKeep :: (a -> Bool) -> [a] -> [[a]]
listSplitKeep _ [] = [[]]
listSplitKeep pred l = 
    case (break pred l) of
        (lf, [])   -> lf : []
        (lf, r:rs) -> lf : [r] : (listSplitKeep pred rs)

evenIndices :: [a] -> [a]
evenIndices (x : _ : xs) = x : evenIndices xs
evenIndices [x] = [x]
evenIndices [] = []

oddIndices :: [a] -> [a]
oddIndices (_ : x : xs) = x : oddIndices xs
oddIndices _ = []

combineHeaders :: [[a]] -> [[a]]
combineHeaders ([]:ls) = combineHeaders ls
combineHeaders ls = zipWith interleave (map head (evenIndices ls)) (oddIndices ls)

interleave :: a -> [a] -> [a]
interleave betweener l = foldl (++) [] (map (\x -> [betweener,x]) l)

dropEmpty :: [Int] -> [Int]
dropEmpty (1 : 0 : rest) = dropEmpty rest
dropEmpty (x:rest) = x : dropEmpty rest
dropEmpty [] = []

prependll :: a -> [[a]] -> [[a]]
prependll prefix [] = [[prefix]]
prependll prefix (l:ls) = [prefix : l]

binSplit :: [a] -> Int -> (a -> Int) -> [[a]]
binSplit (y:ys) binSize sizeof = go ys binSize sizeof [[y]]
    where
        go [] _ _ binList = binList
        go (x:xs) binSize sizeof binList
            | totalsize + (sizeof x) <= binSize = go xs binSize sizeof (ib ++ [(b ++ [x])])
            | otherwise = go xs binSize sizeof (ib ++ [b] ++ [[x]])
            where
                totalsize = sum (map sizeof b)
                b = last binList
                ib = init binList

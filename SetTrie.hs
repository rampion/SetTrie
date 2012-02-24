module SetTrie where

import Control.Arrow
import Prelude hiding (lookup)
import Data.List (intercalate, unfoldr)
import qualified Data.List as L
import Data.Set hiding (empty, insert, map, fromList, member, delete)
import qualified Data.Set as S
import Data.Map hiding (empty, insert, map, fromList, member, delete, update)
import qualified Data.Map as M

data SetTrie k = SetTrie { 
  support      :: Int, 
  conditioned  :: Map k Int, 
  branches     :: [(k, SetTrie k)]
}

prefixThenIndent :: String -> [String] -> [String]
-- create a line
prefixThenIndent initial [] = [initial]
-- prefix the first line, then indent the rest
prefixThenIndent initial ls = zipWith (++) (initial : repeat spaces) ls
  where spaces = map (const ' ') initial

instance (Show k) => Show (SetTrie k) where
  show = intercalate "\n" . nodeLines
    where nodeLines (SetTrie s cm bs) = prefixThenIndent (show s) $ concatMap edgeLines bs
          edgeLines (k,t) = prefixThenIndent (" -" ++ show k ++ "-> ") $ nodeLines t

empty :: SetTrie k
empty = SetTrie 0 M.empty []

insert :: Ord k => Set k -> SetTrie k -> SetTrie k
insert = update 1

delete :: Ord k => Set k -> SetTrie k -> SetTrie k
delete = update (negate 1)

update :: Ord k => Int -> Set k -> SetTrie k -> SetTrie k
update n ks (SetTrie s cm bs) = SetTrie (s + n) cm' (compress cm' bs')
  where cm' = S.fold (insertWith (+) `flip` n) cm ks
        bs' = case break (flip S.member ks . fst) bs of
                (xs, (k,t):ys) -> xs ++ (k, update n (S.delete k ks) t) : ys
                (_, [])        -> bs ++ S.fold go [] ks
        go k []                         = [(k, SetTrie n M.empty            [])]
        go k bs@[(k', SetTrie _ cm _)]  = [(k, SetTrie n (M.insert k' n cm) bs)]

-- what if I used sets as edge labels?

compress :: Ord k => Map k Int -> [(k, SetTrie k)] -> [(k, SetTrie k)]
compress = curry $ unfoldr nextBranch

nextBranch :: Ord k => (Map k Int, [(k, SetTrie k)]) -> Maybe ( (k, SetTrie k), (Map k Int, [(k, SetTrie k)]) )
nextBranch (cm, bs) | M.null cm && L.null bs = Nothing
                    | M.null cm || L.null bs = error "should never happen"
                    | otherwise = 
                        let (k, cm') = deleteByMaxValue cm
                            (t, bs') = extractContaining k bs
                        in Just ( (k, t), (condition t cm', bs') )

condition :: Ord k => SetTrie k -> Map k Int -> Map k Int
condition (SetTrie _ m' _) m = M.filter (/=0) . M.unionWith (+) m $ fmap negate m'

deleteByMaxValue :: Ord k => Map k Int -> (k, Map k Int)
deleteByMaxValue = undefined

extractContaining :: Ord k => k -> [(k, SetTrie k)] -> (SetTrie k, [(k, SetTrie k)])
extractContaining k [] = (empty, []) 
extractContaining k ((k', t@(SetTrie s cm bs)):ts) 
  | k == k'                   = (t, ts)
  | M.lookup k cm == Nothing  = second ((k',t):) $ extractContaining k ts
  | otherwise                 = 
      let (u, bs')  = extractContaining k bs
          cm'       = condition u cm
          s'        = M.fold (+) 0 cm'
          t'        = SetTrie s' cm' bs'
      in (unify k' u) *** ((k',t'):) $ extractContaining k ts

-- really should have a version of compress that takes two sets of branches, 
-- then we could unite them at once
unify :: Ord k => k -> SetTrie k -> SetTrie k -> SetTrie k
unify k t@(SetTrie s cm bs) (SetTrie s' cm' bs') = SetTrie (s + s') cm'' (compress cm'' bm'')
  where cm'' = M.unionWith (+) cm' $ M.insertWith (+) k s cm'
        bm'' = case break ((==k) . fst) bs' of 
                (xs, (_,t):ys) -> xs ++ (k, L.foldr (uncurry unify) t bs) : ys
                (_, [])        -> bs' ++ [(k,t)]

-- most frequent item is first branch
-- next branch is most frequent conditional on the absense of the earlier
--
--  first branch should be for max of cm
--  second branch for max of (cm - cm_first_branch)
--  third branch for max of (cm - cm_first_branch - cm_second_branch ...)


member :: Ord k => Set k -> SetTrie k -> Bool
member ks (SetTrie s cm bs) | S.null ks = Just s /= fmap fst (M.maxView cm)
                            | otherwise = foldr go False bs
  where go (k,t) l = if S.member k ks then member (S.delete k ks) t else l

fromList :: Ord k => [Set k] -> SetTrie k
fromList = foldr insert empty

example :: SetTrie Char
example = fromList [s_abcd, s_abc, s_ab, s_a, s_acd, s_]

s_abcd, s_abc, s_ab, s_a, s_acd, s_ :: Set Char
[s_abcd, s_abc, s_ab, s_a, s_acd, s_] =  [ S.fromList "abcd"
                                         , S.fromList "abc"
                                         , S.fromList "ab"
                                         , S.fromList "a"
                                         , S.fromList "acd"
                                         , S.fromList ""
                                         ]

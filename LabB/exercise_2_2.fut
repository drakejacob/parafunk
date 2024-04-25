-- ==
-- input @ one_100_i32s
-- input @ one_1000_i32s
-- input @ one_10000_i32s
-- input @ one_100000_i32s
-- input @ one_1000000_i32s
-- input @ one_5000000_i32s
-- input @ one_10000000_i32s

import "lib/github.com/diku-dk/sorts/radix_sort"

def segscan [n] 't (op: t -> t -> t) (ne: t) (arr: [n](t, bool)): *[n]t =
    let op' (v1, f1) (v2, f2): (t, bool) =
        (if f2 then v2 else op v1 v2, f1 || f2)
    let result = scan op' (ne, false) arr
    in map (\(v, _) -> v) result

def segreduce [n] 't (op: t -> t -> t) (ne: t) (arr: [n](t, bool)): *[]t =
    let scanned = segscan op ne arr
    let flags = map (\(_, i) -> i) arr -- [false, true, ...., false]
    let indices = map (\i -> if i == n-1 || flags[i+1] then i else -1) (iota n) -- get indices of segment ends (non-ends have -1)
    let only_ends = filter (\x -> x != -1) indices   -- get indices of ONLY segment ends
    in map (\idx -> scanned[idx]) only_ends 

def main (xs:[]i32) (ys:[]bool) = 
    -- segscan (+) 0 (zip xs ys) 
    -- scan (+) 0 xs
    -- segreduce (+) 0 (zip xs ys) 
    reduce (+) 0 xs

def hist 'a [n]
    (op : a -> a -> a) (ne : a)
    (bin_count: i64) (is : [n]i64) (as : [n]a) : []a =
     
    let max_element = reduce i64.max i64.lowest is
    let min_element = reduce i64.min i64.highest is
    let bin_size = (max_element - min_element) / bin_count
    let keying (i:i64) : i64 = 
        if i == max_element then bin_count - 1 else (i - min_element) / bin_size

    let key_list = map keying is 
    let pairs = zip key_list as

    let sorted = radix_sort_by_key (\(k, _) -> k) 64 i64.get_bit pairs
    let sorted_keys = map (\(k, _) -> k) sorted
    let sorted_vals = map (\(_, v) -> v) sorted
    let flags = map2 (\k i-> if i == 0 then true 
        else (if sorted_keys[i-1] == k then false else true)) sorted_keys (iota n)

    let segments = zip sorted_vals flags
    let reduced = segreduce op ne segments

    let ind_of_keys = filter (\i -> flags[i]) (iota n) -- [0,9]
    let indices = map (\i -> sorted_keys[i]) ind_of_keys

    in scatter (replicate bin_count ne) indices reduced
    -- in reduced
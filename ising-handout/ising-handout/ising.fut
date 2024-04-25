-- We represent a spin as a single byte.  In principle, we need only
-- two values (-1 or 1), but Futhark represents booleans a a full byte
-- entirely, so using an i8 instead takes no more space, and makes the
-- arithmetic simpler.
type spin = i8

import "lib/github.com/diku-dk/cpprandom/random"

-- Pick an RNG engine and define random distributions for specific types.
module rng_engine = minstd_rand
module rand_f32 = uniform_real_distribution f32 rng_engine
module rand_i8 = uniform_int_distribution i8 rng_engine

-- We can create an few RNG state with 'rng_engine.rng_from_seed [x]',
-- where 'x' is some seed.  We can split one RNG state into many with
-- 'rng_engine.split_rng'.
--
-- For an RNG state 'r', we can generate random integers that are
-- either 0 or 1 by calling 'rand_i8.rand (0i8, 1i8) r'.
--
--   
-- Remember to consult
-- https://futhark-lang.org/pkgs/github.com/diku-dk/cpprandom/latest/

def rand = rand_f32.rand (0f32, 1f32)
def rand_i = rand_i8.rand (0i8, 1i8)

-- Create a new grid of a given size.  Also produce an identically
-- sized array of RNG states.
def random_grid (seed: i32) (h: i64) (w: i64)
              : ([h][w]rng_engine.rng, [h][w]spin) =
  
  let first_rng = rng_engine.rng_from_seed [seed]

  let rngs = rng_engine.split_rng (h*w) first_rng

  let spins = map (\r -> let (_, random_value) = rand r
                         in if random_value < 0.5 then -1:spin else 1:spin) rngs
  
  in (unflatten rngs, unflatten spins)

-- Compute $\Delta_e$ for each spin in the grid, using wraparound at
-- the edges.
def deltas [h][w] (spins: [h][w]spin): [h][w]i8 =
  let rotate_up = rotate (-1) spins         -- Move all rows up by 1
  let rotate_down = rotate 1 spins          -- Move all rows down by 1 
  let rotate_left = map (rotate (-1)) spins -- Move all columns left by 1
  let rotate_right = map (rotate 1) spins   -- Move all columns right by 1

  in map5 (\c u d l r ->
      map5 (\center up down left right ->
        2 * center * (up + down + left + right)
      ) c u d l r
    ) spins rotate_up rotate_down rotate_left rotate_right

-- Take one step in the Ising 2D simulation.
def step [h][w] (abs_temp: f32) (samplerate: f32)
                (rngs: [h][w]rng_engine.rng) (spins: [h][w]spin)
              : ([h][w]rng_engine.rng, [h][w]spin) =
  
  let delta_grid = deltas spins

  let mat = map2 (\rng_row (delta_row, spin_row) ->
      map2 (\rng (delta, spin) ->
        let (a, b) = rand rng
        let (a2, c) = rand a
        let x = f32.exp (-(f32.i8 delta) / abs_temp)
        let flip = c < samplerate && (delta < -delta || b < x)
        let new_spin = if flip then -spin else spin
        in (a2, new_spin)
      ) rng_row (zip delta_row spin_row)
    ) rngs (zip delta_grid spins)
  let (new_rngs, new_spins) = unzip (flatten mat)

  in (unflatten new_rngs, unflatten new_spins) 

-- | Just for benchmarking.
def main (abs_temp: f32) (samplerate: f32)
         (h: i64) (w: i64) (n: i32): [h][w]spin =
  (loop (rngs, spins) = random_grid 1337 h w for _i < n do
     step abs_temp samplerate rngs spins).1

-- ==
-- entry: main
-- input { 0.5f32 0.1f32 10i64 10i64 2 } auto output

-- ==
-- entry: main
-- input { 0.5f32 0.1f32 10i64 10i64 100 } auto output

-- ==
-- entry: main
-- input { 0.5f32 0.1f32 100i64 100i64 2 } auto output

-- ==
-- entry: main
-- input { 0.5f32 0.1f32 1000i64 1000i64 2 } auto output

-- ==
-- entry: main
-- input { 0.5f32 0.1f32 1000i64 1000i64 10 } auto output

-- The following definitions are for the visualisation and need not be modified.

type~ state = {cells: [][](rng_engine.rng, spin)}

entry tui_init seed h w : state =
  let (rngs, spins) = random_grid seed h w
  in {cells=map (uncurry zip) (zip rngs spins)}

entry tui_render (s: state) = map (map (.1)) s.cells

entry tui_step (abs_temp: f32) (samplerate: f32) (s: state) : state =
  let rngs = (map (map (.0)) s.cells)
  let spins = map (map (.1)) s.cells
  let (rngs', spins') = step abs_temp samplerate rngs spins
  in {cells=map (uncurry zip) (zip rngs' spins')}

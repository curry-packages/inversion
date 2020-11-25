--- ----------------------------------------------------------------------------
--- This module provides some utility functions for inverting functions.
---
--- @author Michael Hanus
--- @version November 2020
--- ----------------------------------------------------------------------------

module Data.Function.Inversion where

--- Inverts a unary function.
invf1 :: Data a => (a -> b) -> (b -> a)
invf1 f y | f x =:<= y  = x where x free

--- Inverts a binary function.
invf2 :: (Data a, Data b) => (a -> b -> c) -> (c -> (a,b))
invf2 f y | f x1 x2 =:<= y  = (x1,x2) where x1,x2 free

--- Inverts a ternary function.
invf3 :: (Data a, Data b, Data c) => (a -> b -> c -> d) -> (d -> (a,b,c))
invf3 f y | f x1 x2 x3 =:<= y  = (x1,x2,x3) where x1,x2,x3 free

--- Inverts a function of arity 4.
invf4 :: (Data a, Data b, Data c, Data d) =>
         (a -> b -> c -> d -> e) -> (e -> (a,b,c,d))
invf4 f y | f x1 x2 x3 x4 =:<= y  = (x1,x2,x3,x4) where x1,x2,x3,x4 free

--- Inverts a function of arity 5.
invf5 :: (Data a, Data b, Data c, Data d, Data e) =>
         (a -> b -> c -> d -> e -> f) -> (f -> (a,b,c,d,e))
invf5 f y | f x1 x2 x3 x4 x5 =:<= y = (x1,x2,x3,x4,x5) where x1,x2,x3,x4,x5 free

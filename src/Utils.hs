module Utils where

import Data.Word

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
infixl 0 |>

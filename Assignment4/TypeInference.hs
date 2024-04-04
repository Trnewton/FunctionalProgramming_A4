module TypeInference where

import Lib.AST

-- | autoGraderTypeInference 
-- You should implement this function! This function should take a LambdaTerm
-- and output either:
--      - Left: an informative error message (either a free variable error,
--          or a type inferencing error such as match failure or occurs check)
--      - Right: the most general inferred type of the lambda term
-- Wahoo!! Last assignment you're nearly there :D
autoGraderTypeInference :: LambdaTerm -> Either String LambdaTyTerm
autoGraderTypeInference = undefined

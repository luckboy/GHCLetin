{-# LANGUAGE CPP, TemplateHaskell #-}
module Main (main) where
#ifndef TEST
main = putStrLn "Hello world!!!"
#else
import Test.QuickCheck.All (quickCheckAll)
prop_first s = s == s
main = $quickCheckAll
#endif

module SubTypeTests where

import STLCExtensions
import Test.HUnit

-- run the test suite: runTestTT testSuite

testSuite = TestList [ 
    TestLabel "Record: r1 <: r3" rec1,
    TestLabel "Record: r1 <: r2" rec2,
    TestLabel "Record: RecWidth - r2 <: r1" rec3,
    TestLabel "Record: r1 <: r2" rec4,
    TestLabel "Record: RecPerm - r2 <: r2perm" rec5,
    TestLabel "Record: RecPerm - r2perm <: r2" rec6
  ]

r1 = TRecord [("x", TInt)]
r2 = TRecord [("x", TInt), ("y", TString)]
r2perm = TRecord [("y", TString), ("x", TInt)]
r3 = TRecord [("x", TTop)]

  
rec1 = TestCase $ assertBool "TInt should be subtype of TTop" $ r1 <: r3

rec2 = TestCase $ assertEqual "TTop should not be subtype of TInt" (r3 <: r1) False

rec3 = TestCase $ assertBool "Record with at least all elements of super type should be a subtype" $ r2 <: r3

rec4 = TestCase $ assertEqual "Record that doesn't have all elements of super type should not be a subtype" (r1 <: r2) False

rec5 = TestCase $ assertBool "Order of elements should not matter in a subtype relation for records" $ r2 <: r2perm

rec6 = TestCase $ assertBool "Order of elements should not matter in a subtype relation for records" $ r2perm <: r2
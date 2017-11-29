module SubTypeTests where

import STLCExtensions
import Test.HUnit

-- run the test suite: runTestTT testSuite

testSuite = TestList [ 
    TestLabel "Record: r1 <: r3" rec1,
    TestLabel "Record: r1 <: r2" rec2,
    TestLabel "Record: RcdWidth - r2 <: r1" rec3,
    TestLabel "Record: r1 <: r2" rec4,
    TestLabel "Record: RcdPerm - r2 <: r2perm" rec5,
    TestLabel "Record: RcdPerm - r2perm <: r2" rec6,
    TestLabel "Record: RcdDepth - r4 <: r5" rec7,
    TestLabel "Record: RcdDepth - r5 <: r4" rec8
  ]

r1 = TRecord [("x", TInt)]
r2 = TRecord [("x", TInt), ("y", TString)]
r2perm = TRecord [("y", TString), ("x", TInt)]
r3 = TRecord [("x", TTop)]
innerRecX = TRecord [("a", TInt), ("b", TInt)]
innerRecY = TRecord [("m", TInt), ("n", TInt)]
r4 = TRecord [("x", innerRecX), ("y", innerRecY)]
r5 = TRecord [("x", TRecord [("a", TInt)]), ("y", TRecord [("n", TInt)]) ]
  
rec1 = TestCase $ assertBool "TInt should be subtype of TTop" $ r1 <: r3

rec2 = TestCase $ assertEqual "TTop should not be subtype of TInt" (r3 <: r1) False

rec3 = TestCase $ assertBool "Record with at least all fields of super type should be a subtype" $ r2 <: r3

rec4 = TestCase $ assertEqual "Record that doesn't have all fields of super type should not be a subtype" (r1 <: r2) False

rec5 = TestCase $ assertBool "Order of fields should not matter in a subtype relation for records" $ r2 <: r2perm

rec6 = TestCase $ assertBool "Order of fields should not matter in a subtype relation for records" $ r2perm <: r2

rec7 = TestCase $ assertBool "Corresponding fields should keep a subtype relation" $ r4 <: r5

rec8 = TestCase $ assertEqual "Corresponding fields should keep a subtype relation" (r5 <: r4) False
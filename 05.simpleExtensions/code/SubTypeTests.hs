module SubTypeTests where

import STLCExtensions
import Test.HUnit

-- run the test suite: runTestTT testSuite
runTests = runTestTT testSuite

testSuite = TestList [ 
    TestLabel "Record: r2 <: r" recc,
    TestLabel "Record: r1 <: r3" rec1,
    TestLabel "Record: r1 <: r2" rec2,
    TestLabel "Record: RcdWidth - r2 <: r1" rec3,
    TestLabel "Record: r1 <: r2" rec4,
    TestLabel "Record: RcdPerm - r2 <: r2perm" rec5,
    TestLabel "Record: RcdPerm - r2perm <: r2" rec6,
    TestLabel "Record: RcdDepth - r4 <: r5" rec7,
    TestLabel "Record: RcdDepth - r5 <: r4" rec8,
    TestLabel "Record: Transitivity - recA <: recB, recB <: recC, recA <: recC" recTrans,
    TestLabel "Record: Reflexivity" recRefl,
    TestLabel "TBool <: TBool" reflTBool,
    TestLabel "TInt <: TInt" reflTInt, 
    TestLabel "TString <: TString" reflTString,
    TestLabel "arr <: arr" reflArr,
    TestLabel "Arrow: arr1 <: arr" arrow1,
    TestLabel "Arrow: arr2 <: arr" arrow2,
    TestLabel "Arrow: arr3 <: arr " arrow3,
    TestLabel "Transitivity with Primitive Types" trans
  ]

r = TRecord [("y", TString)]
r1 = TRecord [("x", TInt)]
r2 = TRecord [("x", TInt), ("y", TString)]
r2perm = TRecord [("y", TString), ("x", TInt)]
r3 = TRecord [("x", TTop)]
innerRecX = TRecord [("a", TInt), ("b", TInt)]
innerRecY = TRecord [("m", TInt), ("n", TInt)]
r4 = TRecord [("x", innerRecX), ("y", innerRecY)]
r5 = TRecord [("x", TRecord [("a", TInt)]), ("y", TRecord [("n", TInt)]) ]

-- Transitivity on Records
recA = TRecord [("v1", TInt), ("v2", TString), ("v3", TBool)]
recB = TRecord [("v3", TBool), ("v2", TString)]
recC = TRecord [("v2", TString)]

-- T3 <: T1      T2 <: T4
-- ----------------------
-- T1 -> T2  <:  T3 -> T4
arr  = TArrow  (TRecord [("x", TInt), ("y", TInt), ("z", TInt)]) (TRecord [("x", TInt), ("y", TInt)])
arr1 = TArrow (TRecord [("x", TInt), ("y", TInt)]) (TRecord [("x", TInt), ("y", TInt), ("w", TBool)])
arr2 = TArrow (TRecord [("y", TInt)]) (TRecord [("w", TBool)])
arr3 = TArrow (TRecord [("w", TInt)]) (TRecord [("x", TInt), ("y", TInt)])

-- Test cases

recc = TestCase $ assertEqual "{x: Int, y: String} should be subtype of {y: String}" True (r2 <: r)
  
rec1 = TestCase $ assertEqual "{x: Top} should not be subtype of {x: Int}" True (r1 <: r3)

rec2 = TestCase $ assertEqual "{x: Int} should be subtype of {x: Top}" False (r3 <: r1)

rec3 = TestCase $ assertEqual "Record with at least all fields of super type should be a subtype" True (r2 <: r3)

rec4 = TestCase $ assertEqual "Record that doesn't have all fields of super type should not be a subtype" False (r1 <: r2)

rec5 = TestCase $ assertEqual "Order of fields should not matter in a subtype relation for records" True (r2 <: r2perm)

rec6 = TestCase $ assertEqual "Order of fields should not matter in a subtype relation for records" True (r2perm <: r2)

rec7 = TestCase $ assertEqual "Corresponding fields should keep a subtype relation" True (r4 <: r5)

rec8 = TestCase $ assertEqual "Corresponding fields should keep a subtype relation" False (r5 <: r4)

recTrans = TestCase $ do assertEqual "recA should be subtype of recB" True (recA <: recB)
                         assertEqual "recB should be subtype of recC" True (recB <: recC)
                         assertEqual "recA should be subtype of recC" True (recA <: recC)

recRefl = TestCase $ do assertEqual "A record should be subtype/supertype of itself" True (recA <: recA)
                        assertEqual "A record should be subtype/supertype of itself" True (recB <: recB)
                        assertEqual "A record should be subtype/supertype of itself" True (recC <: recC)                        
                        assertEqual "A record should be subtype/supertype of itself" True (r <: r)
                        assertEqual "A record should be subtype/supertype of itself" True (r1 <: r1)                        
                        assertEqual "A record should be subtype/supertype of itself" True (r2 <: r2)                        
                        assertEqual "A record should be subtype/supertype of itself" True (r3 <: r3)                        
                        assertEqual "A record should be subtype/supertype of itself" True (r4 <: r4)
                        assertEqual "A record should be subtype/supertype of itself" True (r5 <: r5)  
                        
reflTInt = TestCase $ assertEqual "TInt should be subtype of itself" True (TInt <: TInt)
reflTBool = TestCase $ assertEqual "TInt should be subtype of itself" True (TBool <: TBool)
reflTString = TestCase $ assertEqual "TInt should be subtype of itself" True (TString <: TString)

reflArr = TestCase $ assertEqual "arr should be subtype of itself" True (arr <: arr)
arrow1 = TestCase $ assertEqual "Should be subtype" True (arr1 <: arr)
arrow2 = TestCase $ assertEqual "Should not be subtype" False (arr2 <: arr)
arrow3 = TestCase $ assertEqual "Should not be subtype" False (arr3 <: arr)

trans = TestCase $ do assertEqual "TBool <: TInt should return true (sub_sup list of tuples)" True (TBool <: TInt)
                      assertEqual "TInt <: TNumber should return true (sub_sup list of tuples)" True (TInt <: TNumber)
                      assertEqual "TBool <: TNumber should return true (TRANSITIVITY)" True (TBool <: TNumber)

---
author: Tom Sydney Kerckhove
title: Practical Property Testing
date: 2020-11-05
---

# Outline

- What
- Why
- Relevant internal workings
- Getting started!
- Combinators
- Property testing of a web service with DB
- Writing your own generators
- Links to relevant projects


# What is property testing?

Unit test:

``` haskell
reverse (reverse [1, 7, 3]) `shouldBe` [1, 7, 3]
```

<!--
  Here's a unit test.
  We apply some function to an example input, and we declare that we expect a given output.

  A unit test is just a piece of code that you run.
  If the code crashes, then we say that the test fails, if it doesn't, then we say that the test passes.
-->

. . .

Property test:

``` haskell
\ls -> reverse (reverse ls) `shouldBe` ls
```

<!--
  Here's a property test.
  A property test is also a piece of code, but now it has an argument.
  In randomised property testing, as we'll discuss today, we randomly generate
  the argument to this piece of code.
-->

# Why do we use property testing?

```
+ Cheap examples
+ Smart examples
+ Edge cases
```

. . .

```
- Have to come up with properties
- Generators & Shrinking
```

<!--
- Coming up with properties instead of unit tests
  can be harder in the beginning.
  
- Now we have to worry about generators and shrinking
-->

# Example

```
> quickCheck $ \ls ->
    reverse (reverse ls) `shouldBe` ls
+++ OK, passed 100 tests.
```

<!--
  Here is an example of how to run a property test
  with QuickCheck and HSpec
-->


# Internal workings: Generator

```

  Seed
   |
   v
+-------+    +---+
| Gen A | -> | A |
+-------+    +---+
   ^
   |
  Size

```

<!--
  A generator of 'A's generates 'A's. Simple.

  Generators use a seed to guarantee determinism.
  And you should make sure that your tests use a fixed seed to help make sure they are reproducible.

  Generators also use a size parameter to guide the generation.
  For example, you cannot generate a random list uniformly from the set of all lists, because that
  set is arbitrarily large. So Generators use the size parameter to guide this generation.
-->


# Internal workings: Shrinking

What happens if a property test fails?

. . .

```
-- With shrinking turned off:
quickCheck $ \s -> all isUpper $ map toUpper s
*** Failed! Falsifiable (after 1 test):  
"x\CAN\32937J\ENQ^\DC1?\FS\96943\&0\74134V+"
```

. . .

- Try to shrink: `A -> [A]`

<!--

  When a test fails, QuickCheck will try to 
  shrink the counterexample to something more manageable

-->

. . .

```
quickCheck $ \s -> all isUpper $ map toUpper s
*** Failed! Falsifiable (after 3 tests and 1 shrink):     
"1"
```

# Getting started


``` haskell
spec :: Spec
spec =
  describe "reverse" $
    specify "reversing twice is the same as not reversing at all" $
      forAllValid $ \ls -> reverse (reverse ls) `shouldBe` ls
```

# Default Generators

``` haskell
forAllValid :: (GenValid a, Testable prop) => (a -> prop) -> Property
genValid :: GenValid a => Gen a
```

# Free Generators

``` haskell
instance Validity FooBar
instance GenUnchecked FooBar
instance GenValid FooBar
```

. . .

``` haskell
instance GenValid FooBar where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
```

# Getting started: Integration tests


``` haskell
spec :: Spec
spec = 
  describe "main"
    specify "does not crash"
      forAllValid $ \args ->
        withArgs main args :: IO ()
```

# Getting started: What to test?

``` haskell
shouldBeValid :: (Show a, Validity a) => a -> IO ()
```

. . .

``` haskell
spec :: Spec
spec = 
  describe "returnAThing"
    specify "returns a thing"
      forAllValid $ \input -> do
        thing <- returnAThing input
        shouldBeValid thing
```

<!--

  PAUSE FOR QUESTIONS

-->

# Property combinators

``` haskell
spec :: Spec
spec = do
  describe ">=" $
    it "is reflexive" $
      forAllValid $ \a ->
        (a >= a) `shouldBe` True
  describe "<=" $
    it "is reflexive" $
      forAllValid $ \a ->
        (a <= a) `shouldBe` True
```

. . .
 
``` haskell
reflexivity :: GenValid a => (a -> a -> Bool) -> Property 
reflexivity op = forAllValid $ \a -> (a `op` a) `shouldBe` True
```

# Property combinators
 

Usage:

``` haskell
spec :: Spec
spec = do
  describe ">=" $
    it "is reflexive" $ 
      reflexivity (>=)
  describe "<=" $
    it "is reflexive" $ 
      reflexivity (<=)
```

# Property combinators

``` haskell
producesValidsOnValids
  :: (GenValid a, Valid b)
  => (a -> b) -> Property
```

. . .


``` haskell
computeAThing :: Input -> Thing

spec :: Spec
spec = do
  describe "computeAThing" $
    it "computes a thing" $ 
      producesValidsAnValids computeAThing
```


# Test suite combinators

Definition:

``` haskell
lensSpec :: (GenValid a, GenValid b) => Lens' a b -> Spec
```

. . .

Usage:

``` haskell
spec :: Spec
spec = 
  describe "myLens" $ 
    lensSpec myLens
```

# Test suite combinators

Output:

```
  myLens
    satisfies the first lens law
      +++ OK, passed 100 tests.
    satisfies the second lens law
      +++ OK, passed 100 tests.
    satisfies the third lens law
      +++ OK, passed 100 tests.
```


<!--

  PAUSE FOR QUESTIONS

-->

<!--

  Now let's have a look at testing a real-world application

-->

# Property testing with a web service and/or database

```
+------+    +-------------+    +----------+
| Test | -> | Web Service | -> | Database |
+------+    +-------------+    +----------+
```

. . .

Plan:

- Set up everything that's necessary for tests
  Do this once, beforehand, or around every test.

- Run the tests

- Tear everything down, afterward, or around every test.

# Property testing with a web service and/or database

``` haskell
withMyServer :: SpecWith ClientEnv -> Spec
withMyServer = 
    aroundWith withMyApp 
  . beforeAll (HTTP.newManager defaultManagerSettings)
```

. . .

``` haskell
withMyApp :: (ClientEnv -> IO a) -> HTTP.Manager -> IO a
withMyApp func man = do
    Warp.testWithApplication setupMyApp $ \port ->
        func $ ClientEnv man (BaseUrl Http "127.0.0.1" port "")
```

# Property testing with a web service and/or database

. . .

<!--
  Let's assume your webservice has an api with a single
  api endpoint. We can now write a test that checks that
  you get the same result every time, for any argument 
  that you can pass to the API call.
-->

``` haskell
callMyAPI :: MyArg -> ClientM MyResult
runClient :: ClientEnv -> ClientM a -> IO a
```
. . .
``` haskell
spec :: Spec
spec = withMyServer $ 
  describe "calling the api"
    it "gets the same result twice for any argument" $
      \clientEnv ->
        forAllValid $ \arg -> do
          (r1, r2) <- runClient clientEnv $ do
              r1 <- callMyAPI arg
              r2 <- callMyAPI arg
              pure (r1, r2)
          r1 `shouldBe` r2
```

<!--
  Here is the code that's necessary.
  Let's break it down a bit so that you know how to read this.
-->

# Property testing with a web service and/or database

``` haskell
            
                      
                            
                                                      
                   
                                     
          (r1, r2) <- runClient clientEnv $ do
              r1 <- callMyAPI arg
              r2 <- callMyAPI arg
              pure (r1, r2)
          r1 `shouldBe` r2
```

# Property testing with a web service and/or database

``` haskell
            
                      
                            
                                                      
                   
        forAllValid $ \arg -> do
          (r1, r2) <- runClient clientEnv $ do
              r1 <- callMyAPI arg
              r2 <- callMyAPI arg
              pure (r1, r2)
          r1 `shouldBe` r2
```

# Property testing with a web service and/or database

``` haskell
            
                      
  describe "calling the api"
    it "gets the same result twice for any argument" $
      \clientEnv ->
        forAllValid $ \arg -> do
          (r1, r2) <- runClient clientEnv $ do
              r1 <- callMyAPI arg
              r2 <- callMyAPI arg
              pure (r1, r2)
          r1 `shouldBe` r2
```

# Property testing with a web service and/or database

``` haskell
spec :: Spec
spec = withMyServer $ 
  describe "calling the api"
    it "gets the same result twice for any argument" $
      \clientEnv ->
        forAllValid $ \arg -> do
          (r1, r2) <- runClient clientEnv $ do
              r1 <- callMyAPI arg
              r2 <- callMyAPI arg
              pure (r1, r2)
          r1 `shouldBe` r2
```


<!--

  PAUSE FOR QUESTIONS

-->


# Custom Generators

Building blocks:

``` haskell
suchThat :: Gen a -> (a -> Bool) -> Gen a
elements :: [a] -> Gen a
```

. . .

Example:
``` haskell
genSuit, genVal :: Gen Char
genSuit = elements "♠♥♦♣"
genVal = elements "123456789JQK"
```

# Custom Generators

Applicative:

``` haskell
genCard :: Gen (Char, Char)
genCard = (,) <$> genSuit <*> genVal
```

. . .

Monad:

``` haskell
genCards :: Gen [(Char, Char)]
genCards = do
  l <- choose (1, 10)
  replicateM l genCard
```

# Using sizes

Building blocks:

``` haskell
sized :: (Int -> Gen a) -> Gen a
resize :: Int -> Gen a -> Gen a
```

...

Example:

``` haskell
genListOf15Ints :: Gen [Int]
genListOf15Ints = resize 15 $ sized $ \n ->
  replicateM n genValid
```


# Using a custom generator

<!--
  Now that we can write custom generators,
  we can start using our custom generators
-->

Without shrinking:

``` haskell
myCustomGenerator :: Gen V
forAll :: Gen a -> (a -> Property) -> Property

forAll myCustomGenerator $ \v -> myPropertyOf v
```

. . .

With custom shrinking:

``` haskell
myShrink :: V -> [V]
forAllShrink :: Gen a -> (a -> Property) -> (a -> [a]) -> Property

forAllShrink myCustomGenerator
  myShrink $ \v -> myPropertyOf v
```

<!--
  We can also use a custom shrinking function.
  Let's have a look at why you would want to do that
-->

# Dangers: shrinking 

``` haskell
forAllShrink
  (genValid `suchThat` (> 5))
  shrink $ \i ->
    i > 5 && even i
```

<!--
  Here is an example of why you would use a custom shrinking function.
  Suppose you think that all numbers greater than 5 have the property 
  that they are greater than 5 and even.
-->

. . .

```
*** Failed! Falsifiable (after 2 tests and 1 shrink):  
0
```

<!--
  When QuickCheck runs the property test, it will
  give you '0' as the counter example.
  This may be a bit confusing because you were only trying to look
  at numbers larger than 5.
  Of course 0 does not satisfy the property, but it doesn't satisfy the
  property for a different reason than the first counterexample that was
  generated.
-->

. . .

=> *Maintain invariants of generator*

<!--
  So the lesson is that you should use a custom shrinking function
  if your generator generates values to satisfy some constraint.
  Then the custom shrinking function must also maintain that constraint.
  You could fix this example by filtering the result of this shrinking function 
  through the predicate (>5).
-->

# Dangers: `suchThat`

``` haskell
pure 100 :: Gen Int
```

``` haskell
arbitrary `suchThat` (== 100) :: Gen Int
```

<!--
  There are some custom gotcha's with writing custom generators as well.
  The first one is one to watch out for whenever you use the suchThat function.

  [Explain difference between these two generators]
-->

. . .

=> *Watch out for*:

- Predicates that are rarely satisfied
- Expensive predicates

<!--
  From experience: usually when property tests are slow, it's because generating the input is slow.
-->

# Dangers: Size 

```
> quickCheck $ \ls ->  length ls < 100
+++ OK, passed 100 tests.
```

<!--
  Here's another thing to watch out for.
  QuickCheck can produce falsely positive results.
  In this case, it seems to claim that every list is
  shorter than 100 elements.
  This is because all of the generated lists are indeed short.
-->

. . .

versus

```
> quickCheck $
    forAll (resize 200 arbitrary) $ \ls ->
      length ls < 100

*** Failed! Falsifiable (after 1 test):  
[...]
```

<!--
  If we use a larger size for the generator,
  we see that this property does not hold.
-->

. . .

=> *Watch out for properties that depend on size.*

# Dangers: Size 

``` haskell
data Tree = Leaf | Branch Tree Tree
    deriving (Show, Eq)
```

``` haskell
instance GenValid Tree where
    genValid = oneof
      [ pure Leaf
      , Branch <$> genValid <*> genValid]
```

``` haskell
size :: Tree -> Int
size Leaf = 1
size (Branch t1 t2) = size t1 + size t2
```

# Dangers: Size 

```
sample (size <$> resize 1 arbitrary)
1
83144
1312
```

. . .

=> *Watch out for generators that use size non-linearly.*

. . .


``` haskell
instance Arbitrary Tree where
    arbitrary = sized $ \n -> case n of
      1 -> pure Leaf
      _ -> do
        (a, b) <- genSplit n
        Branch <$> resize a genValid <*> resize b genValid
```

# Links to relevant projects

- *QuickCheck*: https://hackage.haskell.org/package/QuickCheck
  Property testing

- *hspec*: https://hackage.haskell.org/package/hspec
  Testing framework

- *validity*: https://github.com/NorfairKing/validity
  Free Generators
  Free Shrinking
  Prepared property combinators
  Prepared test suite combinators

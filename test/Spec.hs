

import Lambda_interpreter
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Lambda_Int" $ do
            describe "reduce" $ do
                it "test 1" $
                    (reduce (App (Abs "x" (Var "x")) (Var "a"))) `shouldBe` (Var "a")
                it "test 2" $
                    (reduce (App (Abs "x" (App (Var "x") (Var "x"))) (Abs "x" (Var "x")))) `shouldBe` (Abs "x" (Var "x")) 
                it "test 3" $
                    (reduce (App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y"))) `shouldBe` (Abs "y" (App (Var "y1") (Var "y")))

-- `(λx.x) a` reduces to `a`
-- >>> reduce (App (Abs "x" (Var "x")) (Var "a")) == (Var "a")
-- True

-- `(λx.x x) (λx.x)` reduces to `λx.x`
-- >>>  == 
-- True

-- `(λx.λy.x y) y` reduces to `λy1.y y1`, and not `λy.y y`
-- >>> r == 
-- True
--test1 = TestCase (assertEqual "for term1"  )
--tests = TestList [TestLabel "test1" test1]


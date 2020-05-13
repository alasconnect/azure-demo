module Test.Domain.Todo where
  
--------------------------------------------------------------------------------
import Data.Either (isRight)
import Data.Functor.Identity (runIdentity)
import Test.Hspec
--------------------------------------------------------------------------------
import Domain.Todo
import Models.Todo
--------------------------------------------------------------------------------
import Test.DataAccess.Todo ()
--------------------------------------------------------------------------------

-- If we cared to track some kind of test related ephemeral parameter
-- or the state of a computation, we could run this all in the State
-- monad instead.

spec :: Spec
spec = parallel $ do
  describe "Domain.Todo" $ do
    it "getTodos" $ do
      let r = runIdentity getTodos
      isRight r `shouldBe` True
      case r of
        Right v -> length v `shouldBe` 5
        Left _  -> pure ()
    it "getTodo" $ do
      let r = runIdentity (getTodo (TodoId 1))
      isRight r `shouldBe` True
    it "createTodo" $ do
      let c = TodoC (TodoName "name") (TodoDesc "desc")
      let r = runIdentity (createTodo c)
      isRight r `shouldBe` True
    it "updateTodo" $ do
      let u = TodoU (TodoId 1) (Just $ TodoName "name") (Just $ TodoDesc "desc") Nothing
      let r = runIdentity (updateTodo u)
      isRight r `shouldBe` True

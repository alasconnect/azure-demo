module Api.User where

--------------------------------------------------------------------------------
import GHC.Stack
import Servant
--------------------------------------------------------------------------------
import qualified Domain.User as DA
import Errors
import Models.User
import Runner
import Types
--------------------------------------------------------------------------------

getUsers
   :: HasCallStack
   => AppContext
   -> Handler [UserR]
getUsers ctx =
   handleError ctx (runApp ctx DA.getUsers)

getUser
   :: HasCallStack
   => AppContext
   -> Handler UserR
getUser ctx uid =
   handleError ctx (runApp ctx (DA.getuser uid))

createUser
   :: HasCallStack
   => AppContext
   -> UserC
   -> Handler UserR
createUser ctx uc =
   handleError ctx (runApp ctx (DA.createUser uc))

updateUser
   :: HasCallStack
   => AppContext
   -> UserU
   -> Handler NoContent
updateUser ctx uu = do
   handleError ctx (runApp ctx (DA.updateUser uu))
   pure NoContent
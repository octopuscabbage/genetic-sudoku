{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module API where
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API.ContentTypes
import Servant.Server
import Util
import Control.DeepSeq
import Control.Applicative
import BackTracker.BackTracker
import Data.Aeson
import System.TimeIt
import Control.Monad.IO.Class
import Data.Matrix hiding ((<|>))

type BackTrackerAPI = "solve" :> Capture "puzzle" String :> Get '[JSON] (Maybe [Int])
                      :<|> "time" :> Capture "puzzle" String :> Get '[JSON] Double
                      :<|> "analyze" :> Capture "n" Int :> Capture "puzzle" String :> Get '[JSON] Double

solve p= return $ (map value . toList) <$> runBackTracker p
time p = liftIO $ fst <$> (timeItT $ return $ force $runBackTracker p)

analyze n p= liftIO $ fst <$> (analyzeSolver n p)

server:: Server BackTrackerAPI
server = solve :<|> time :<|> analyze

sudokuAPI::Proxy BackTrackerAPI
sudokuAPI = Proxy

app::Application
app  = serve sudokuAPI server

runServer = run 8081 app

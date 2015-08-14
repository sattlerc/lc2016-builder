module Main where

import Control.Monad

import qualified Data.ByteString.Lazy as BS
import Data.Foldable
import Data.Monoid

import System.FilePath

import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Text.Blaze.Html.Renderer.Pretty as RenderPretty
import qualified Text.Blaze.Html.Renderer.Utf8 as RenderUtf8

import Process


repo :: FilePath
repo = "lc2016"


data Page = Page { page_name :: String  -- Filename without the '.html' suffix.
                 , page_title :: String
                 , page_content :: Html
                 }

index :: Page
index =
  Page { page_name = "index"
       , page_title = "Home"
       , page_content = "Welcome!"
       }

program :: Page
program =
  Page { page_name = "program"
       , page_title = "Scientific Program"
       , page_content = "To be confirmed."
       }

schedule :: Page
schedule =
  Page { page_name = "schedule"
       , page_title = "Schedule"
       , page_content = "To be confirmed."
       }

registration :: Page
registration =
  Page { page_name = "registration"
       , page_title = "Registration"
       , page_content = do
         H.p "Registration has not yet opened."
         H.p "[Insert appropriate link to University of Leeds Webshop.]"
       }

accomodation :: Page
accomodation =
  Page { page_name = "accomodation"
       , page_title = "Accomodation"
       , page_content = "Here is a selection of rain-proof bridges. Please confirm sufficient space underneath before arrival!"
       }

local_info :: Page
local_info =
  Page { page_name = "local-info"
       , page_title = "Local information"
       , page_content = "It is always raining."
       }

-- Order is reflected in the navigation menu.
pages :: [Page]
pages = [ index
        , program
        , schedule
        , registration
        , local_info
        , accomodation
        ]


pageFileName :: Page -> FilePath
pageFileName page = page_name page <.> "html"

generateMenu :: Maybe String -> [Page] -> Html
generateMenu selectedName = H.nav . H.ul . foldMap f where
  f :: Page -> Html
  f page = H.li $ if Just (page_name page)  == selectedName
                  then link ! A.class_ "selected"
                  else link where
                    link = H.a ! A.href (toValue $ pageFileName page) $ toHtml $ page_title page

generatePage :: Page -> [Page] -> Html
generatePage page pages = do
  H.docTypeHtml $ do
    H.head $ do
      H.title $ toHtml $ page_title page
    H.body $ do
      generateMenu (Just (page_name page)) pages
      page_content page

generate :: IO ()
generate = void $ mapM f pages where
  f :: Page -> IO ()
  f page = do
    BS.writeFile
      (repo </> pageFileName page)
      (RenderUtf8.renderHtml $ generatePage page pages)
    

push :: IO ()
push = do
  let git = evalCommand (Just repo) "git"
  git ["add", "-A"]
  git ["commit", "-m", "<empty>"]
  git ["push"]

main :: IO ()
main = do
  generate
  evalCommand (Just repo) "firefox" ["--new-tab", "index.html"]
  return ()

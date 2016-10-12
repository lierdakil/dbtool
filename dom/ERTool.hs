{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}

import Reflex
import Reflex.Dom

import ERTools

import qualified GHCJS.Types    as T
import qualified Data.JSString as JSS

import Reflex.Spider.Internal
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Types (castToHTMLDocument, castToHTMLElement)

graphInput :: MonadWidget t m => m (Dynamic t String)
graphInput = do
  n <- textArea def
  el "br" $ return ()
  bt <- button "Анализ"
  holdDyn "" $ tagDyn (value n) bt

foreign import javascript safe "Viz($1, { format: \"svg\" })"
  vizDot :: T.JSString -> T.JSString

widget :: Widget
            Spider
            (Gui
               Spider
               (WithWebView SpiderHost)
               SpiderHostFrame)
            ()
          -> IO ()
widget w =
  runWebGUI $ \webView -> do
    Just doc <- fmap castToHTMLDocument <$> webViewGetDomDocument webView
    Just elm <- fmap castToHTMLElement <$> getElementById doc "dbtool"
    attachWidget elm webView w

main :: IO ()
main = widget $ el "div" $ do
  inp <- mapDyn parseER =<< graphInput
  _ <- dyn =<< mapDyn outputWidget inp
  return ()

outputWidget :: (Show a, MonadWidget t m) =>
                Either a ER -> m ()
outputWidget (Left err) = el "div" $ text $ show err
outputWidget (Right inp) = do
  _ <- elDynHtml' "div" $ constDyn $ (JSS.unpack . vizDot . JSS.pack . showER) inp
  return ()

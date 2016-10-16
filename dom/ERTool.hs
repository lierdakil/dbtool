{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}

import Reflex
import Reflex.Dom

import ERTools
import FDTools
import ERToFD

import Reflex.Spider.Internal
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Types (castToHTMLDocument, castToHTMLElement)
import Util

graphInput :: MonadWidget t m => m (Dynamic t String)
graphInput = do
  n <- textArea def
  el "br" $ return ()
  bt <- button "Анализ"
  holdDyn "" $ tagDyn (value n) bt

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
  graphImg . showER $ inp
  el "pre" $ do
    text $ graphToString . erToFDs $ inp

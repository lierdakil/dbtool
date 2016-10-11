{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}

import Reflex
import Reflex.Dom
import qualified Data.Set as S

import Lib
import Data.List

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

foreign import javascript safe "Viz($1, { format: \"svg\", engine: \"dot\" })"
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
  inp <- mapDyn parseGraph =<< graphInput
  _ <- dyn =<< mapDyn outputWidget inp
  return ()

simpleBlock :: MonadWidget t m =>
               String -> r -> (r -> Graph) -> m ()
simpleBlock header inp func =
  customBlock header $
    el "pre" $
      text $ graphToString . func $ inp

customBlock :: MonadWidget t m =>
               String -> m b -> m b
customBlock header disp = do
  el "h1" $ text header
  disp

outputWidget :: (Show a, MonadWidget t m) =>
                Either a Graph -> m ()
outputWidget (Left err) = el "div" $ text $ show err
outputWidget (Right inp) = do
  simpleBlock "Полный список ФЗ" inp fullext
  simpleBlock "Полный список ФЗ (кроме тривиальных)" inp (nontrivial . fullext)
  simpleBlock "Суперключи" inp superkeys
  simpleBlock "Потенциальные ключи" inp potkeys
  simpleBlock "Минимальный список ФЗ" inp (minimize . fullext)
  simpleBlock "ФЗ, не удовлетворяющие 3НФ (минимальное множество)" inp $
    minimize . nf3 . fullext
  simpleBlock "ФЗ, не удовлетворяющие НФБК (минимальное множество)" inp $
    minimize . nfbc . fullext
  customBlock "Нормализованные отношения" $
    el "pre" $ do
      let runProject g =
            let norm = normalize g
                projects = map (minimize . flip project (fullext g) . S.fromList) norm
            in zip norm projects
          printProject =
            concatMap p
            where
              p (n, g) = "(" ++ intercalate ", " n ++ "):\n" ++
                graphToString g ++ "\n\n"
      text $ printProject $ runProject inp
  customBlock "Диаграмма атрибутов" $ do
    _ <- elDynHtml' "div" $ constDyn $ (JSS.unpack . vizDot . JSS.pack . printGraph . minimize . fullext) inp
    return ()
  customBlock "Диаграммы атрибутов нормализованных отношений" $
    el "div" $ do
      let runProject g =
            let norm = normalize g
                projects = map (minimize . flip project (fullext g) . S.fromList) norm
            in zip norm projects
          printProject (n, g) = do
            el "p" $ text $ "(" ++ intercalate ", " n ++ "):"
            _ <- elDynHtml' "div" $ constDyn . JSS.unpack . vizDot . JSS.pack . printGraph . minimize . fullext $ g
            return ()
          sp tg = el "div" $
            mapM_ (el "div" . printProject) tg
      sp $ runProject inp
      return ()

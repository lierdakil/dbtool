{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}

import Reflex
import Reflex.Dom
import qualified Data.Set as S

import FDTools
import Data.List

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
  let normalized = normalize inp
  case normalized of
    Left norm -> do
      el "h1" $ text $ show "Некорректная нормализация!"
      normWidget inp norm
    Right norm -> normWidget inp norm

normWidget :: MonadWidget t m =>
              Graph -> [[Vertex]] -> m ()
normWidget inp norm = do
  let prj = map (\x -> (x, minimize . flip project (fullext inp) . S.fromList $ x)) norm
  customBlock "Нормализованные отношения" $
    el "pre" $ do
      let
          printProject =
            concatMap p
            where
              p (n, g) = "(" ++ intercalate ", " (map vtxName n) ++ "):\n" ++
                graphToString g ++ "\n\n"
      text $ printProject prj
  customBlock "Диаграмма атрибутов" $ do
    -- _ <- elDynHtml' "div" $ constDyn $ (JSS.unpack . vizDot . JSS.pack . printGraph . minimize . fullext) inp
    graphImg . printGraph . minimize . fullext $ inp
    return ()
  customBlock "Диаграммы атрибутов нормализованных отношений" $
    el "div" $ do
      let printProject (n, g) = do
            el "p" $ text $ "(" ++ intercalate ", " (map vtxName n) ++ "):"
            graphImg . printGraph . minimize . fullext $ g
            -- _ <- elDynHtml' "div" $ constDyn . JSS.unpack . vizDot . JSS.pack .
            return ()
          sp tg = el "div" $
            mapM_ (el "div" . printProject) tg
      sp prj
      return ()

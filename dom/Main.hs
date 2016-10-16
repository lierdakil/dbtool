{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}

import Reflex
import Reflex.Dom
import qualified Data.Set as S

import FDTools
import Data.List
import Control.Arrow

import Reflex.Spider.Internal

import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Types (castToHTMLDocument, castToHTMLElement)
import Util

graphInput :: MonadWidget t m => m (Dynamic t (String, Bool))
graphInput = do
  n <- textArea def
  c <- checkbox False def
  text "Строгая минимизация (медленно!)"
  el "br" $ return ()
  bt <- button "Анализ"
  v <- combineDyn (,) (value n) (value c)
  holdDyn ("", False) $ tagDyn v bt

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
  inp <- mapDyn (first parseGraph) =<< graphInput
  _ <- dyn =<< mapDyn outputWidget inp
  return ()

simpleBlock :: MonadWidget t m =>
               String -> Graph -> m ()
simpleBlock header g =
  customBlock header $
    el "pre" $
      text $ graphToString g

customBlock :: MonadWidget t m =>
               String -> m b -> m b
customBlock header disp = do
  el "h1" $ text header
  disp

outputWidget :: (Show a, MonadWidget t m) =>
                (Either a Graph, Bool) -> m ()
outputWidget (Left err, _) = el "div" $ text $ show err
outputWidget (Right inp, trueMinimize) = do
  let
    (minimize', minhead)
      | trueMinimize = (minimize, "Минимальный список ФЗ")
      | otherwise = (collect . nontrivial, "Минимальный список ФЗ (неточный)")
    full = fullext inp
    -- nt = nontrivial full
    -- sk = superkeys full
    pk = potkeys full
    minf = conservative minimize' inp
    nf3l = nf3 minf
    nfbcl = nfbc minf
    normalized = normalize minf
  -- simpleBlock "Полный список ФЗ" full
  -- simpleBlock "Полный список ФЗ (кроме тривиальных)" nt
  -- simpleBlock "Суперключи" sk
  simpleBlock "Потенциальные ключи" pk
  simpleBlock minhead minf
  simpleBlock "ФЗ, не удовлетворяющие 3НФ (минимальное множество)" nf3l
  simpleBlock "ФЗ, не удовлетворяющие НФБК (минимальное множество)" nfbcl
  case normalized of
    Left norm -> do
      el "h1" $ text "Некорректная нормализация!"
      normWidget minf norm trueMinimize
    Right norm -> normWidget minf norm trueMinimize

normWidget :: MonadWidget t m =>
              Graph -> [[Vertex]] -> Bool -> m ()
normWidget inp norm trueMinimize = do
  let prj
        | trueMinimize = map (\x -> (x, minimize . flip project (fullext inp) . S.fromList $ x)) norm
        | otherwise = map (\x -> (x, collect . nontrivial . flip project inp . S.fromList $ x)) norm
  customBlock "Нормализованные отношения" $
    el "pre" $ do
      let
          printProject =
            concatMap p
            where
              p n = "(" ++ intercalate ", " (map vtxName n) ++ ")\n"
                -- ++ graphToString g ++ "\n\n"
      text $ printProject norm
  customBlock "Диаграмма атрибутов" $ do
    graphImg . printGraph $ inp
    return ()
  customBlock "Диаграммы атрибутов нормализованных отношений" $
    el "div" $ do
      let printProject (n, g) = do
            el "p" $ text $ "(" ++ intercalate ", " (map vtxName n) ++ "):"
            graphImg . printGraph $ g
            -- _ <- elDynHtml' "div" $ constDyn . JSS.unpack . vizDot . JSS.pack .
            return ()
          sp tg = el "div" $
            mapM_ (el "div" . printProject) tg
      sp prj
      return ()

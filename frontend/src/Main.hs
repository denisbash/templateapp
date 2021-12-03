{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE FlexibleContexts#-}
module Main where

import Reflex.Dom 
import Reflex.Dom.WebSocket
import qualified Data.Text as T 
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.FileEmbed
import Data.Monoid (Endo(..), appEndo)
import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Common 
import Data.Aeson (decodeStrict)
import Data.ByteString (ByteString)
import qualified Control.Lens.Combinators as L

import Language.Javascript.JSaddle.Types
import Reflex.PerformEvent.Class

main :: IO ()
main = mainWidgetWithHead headWidget rootWidgetWS --rootWidget

wSURL = "ws://localhost:8081/stream" :: T.Text

headWidget :: MonadWidget t m => m ()
headWidget = do
  elAttr "meta" ("charset" =: "utf-8") blank
  elAttr "meta"
    (  "name" =: "viewport"
    <> "content" =: "width=device-width, initial-scale=1, shrink-to-fit=no" )
    blank
  elAttr "link"
    (  "rel" =: "stylesheet"
    <> "href" =: "https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css"
    <> "integrity" =: "sha384-Vkoo8x4CGsO3+Hhxv8T/Q5PaXtkKtu6ug5TOeNV6gBiFeWPGFN9MuhOf23Q9Ifjh"
    <> "crossorigin" =: "anonymous")
    blank
  el "title" $ text "Template App"

rowWrapper :: MonadWidget t m => m a -> m a
rowWrapper x = divClass "row justify-content-md-center" $
                divClass "col-9" x

delimiter :: MonadWidget t m => m ()
delimiter = rowWrapper $
                divClass "border-top mt-3" blank

templateWidget'' :: MonadWidget t m => Dynamic t (M.Map NamedTemplate T.Text) -> MTemplate -> m (Event t (Endo MTemplate))
templateWidget'' _ (V Nothing) = return never
templateWidget'' namedTemplatesDyn (V (Just s)) = divClass "d-flex border-bottom" $ do
                          iDyn <- divClass "p-2 flex-grow-1 my-auto" $ do
                            iEl <- inputElement $ def & initialAttributes .~ ("type" =: "text" <> "class" =: "form-control" <> "placeholder" =: (T.pack s))
                              & inputElementConfig_initialValue .~ (T.pack s)  
                              -- & inputElementConfig_setValue .~ ("" <$ btnEv)
                            --let newTemplateDyn = newTemplate <$> value iEl
                            let inptDyn = Endo <$> (const <$> (V . Just . T.unpack <$> _inputElement_value iEl))
                            return inptDyn
                          divClass "p-2 my-auto" $ dynText $ constDyn $ "V"
                         -- dropEv <- divClass "p-2 my-auto" $ showNamedTemplates namedTemplatesDyn
                         -- let dropEndoEv = Endo <$> (const . template <$> dropEv)
                          divClass "p-2 btn-group"$ mdo 
                            let 
                              btnAttr = "class" =: "btn btn-outline-secondary" <> "type" =: "button"
                              btnAttrRed = btnAttr <> "style" =: "color: red"
                              btnAttrGreen = btnAttr <> "style" =: "color: green"
                              redEv = btnAttrRed <$ updated iDyn
                              greenEv = btnAttrGreen <$ btnEndoEv3
                            dynColAttr <- holdDyn btnAttr $ leftmost [redEv, greenEv]
                            (btnEl,_) <- elAttr' "button" btnAttr $ text "Reset"
                            (btnEl2,_) <- elAttr' "button" btnAttr $ text "Remove"
                            (btnEl3,_) <- elDynAttr' "button" dynColAttr $ text "Add"
                            let 
                              btnEv = domEvent Click btnEl
                              btnEv2 = domEvent Click btnEl2
                              btnEv3 = domEvent Click btnEl3
                              btnEndoEv = const (Endo $ resetTemplate) <$> btnEv
                              btnEndoEv2 = (Endo $ const (V Nothing)) <$ btnEv2
                              btnEndoEv3 = tagPromptlyDyn iDyn btnEv3
                            return $ leftmost [btnEndoEv, btnEndoEv2, btnEndoEv3] --, dropEndoEv]

templateWidget'' _ (L Nothing) = return never
templateWidget'' namedTemplates (L (Just ts)) = rowWrapper $ do
        tsEvs <- mapM (templateWidget'' namedTemplates) ts
        let 
          indexedEv = leftmost $ zipWith (\ev i -> (\x -> (x, i)) <$> ev) tsEvs [0..]
          listMapEv = (\(e, i) -> L.over (L.element i) (appEndo e)) <$> indexedEv
          fAux :: ([MTemplate] -> [MTemplate]) -> MTemplate -> MTemplate
          fAux f (L Nothing) = L Nothing
          fAux f (V Nothing) = V Nothing
          fAux f (L (Just mtemps)) = L $ Just $ f mtemps
          fAux f (V (Just s))= (V $ Just s)
          endoEv = Endo . fAux <$> listMapEv
        divClass "p-2 btn-group"$ do 
          let btnAttr = "class" =: "btn btn-outline-secondary" <> "type" =: "button"
          (btnEl,_) <- elAttr' "button" btnAttr $ text "Clear all"
          (btnEl2,_) <- elAttr' "button" btnAttr $ text "Add V"
          (btnEl3,_) <- elAttr' "button" btnAttr $ text "Add L"
          (btnElQ,_) <- elAttr' "button" btnAttr $ text "Add Q"
          let 
            btnEv = domEvent Click btnEl
            btnEndoEv = (Endo $ const (L Nothing)) <$ btnEv
            btnEv2 = domEvent Click btnEl2
            newV = (Endo $ fAux (V ( Just "") :)) <$ btnEv2
            btnEv3 = domEvent Click btnEl3
            newL = (Endo $ fAux (L (Just []) :)) <$ btnEv3
          dynB <- holdDyn False $ leftmost [True <$ domEvent Click btnElQ, False <$ newV, False <$ newL]       
          let dynEv = (\b -> if b then updated <$> namedTemplatesDropdown namedTemplates else return never) <$> dynB
          evEv <- dyn dynEv
          evQ <- switchHold never evEv
          let newQ = Endo <$> (const <$> evQ)
          return $ leftmost [endoEv, btnEndoEv, newV, newL, newQ]

url = "http://localhost:8081/templates" :: T.Text

view :: Maybe T.Text -> T.Text
view mText = maybe "FAILED" id mText

namedTemplatesWidget :: MonadWidget t m => Dynamic t (M.Map NamedTemplate T.Text) -> m (Event t NamedTemplate)
namedTemplatesWidget ntemplatesDyn = do 
    d <- dropdown (NamedTemplate "---select---" Done (V "")) ntemplatesDyn def
    return $ _dropdown_change d
    
namedTemplatesDropdown :: MonadWidget t m => Dynamic t (M.Map NamedTemplate T.Text) -> m (Dynamic t MTemplate)
namedTemplatesDropdown ntemplatesDyn = do 
    d <- dropdown (NamedTemplate "---select---" Done (V "")) ntemplatesDyn def
    return $ toMTemplate . template <$> _dropdown_value d

showNamedTemplates :: MonadWidget t m => Dynamic t (M.Map NamedTemplate T.Text)-> m (Event t NamedTemplate)
showNamedTemplates ntemplatesDyn = el "form" $ do
    cBox <- divClass "col-3" $ inputElement $ def
        & inputElementConfig_initialChecked .~ False
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "checkbox")
    let dV = _inputElement_checked cBox
    divClass "col-3" $ do
      evEv <- dyn $ (\b -> if b then namedTemplatesWidget ntemplatesDyn else return never) <$> dV
      switchHold never evEv
      
createButtons :: MonadWidget t m => Dynamic t (M.Map NamedTemplate T.Text) ->  m (Event t (Endo MTemplate))
createButtons dbTemplates = do
  divClass "p-2 btn-group"$ mdo 
    let btnAttr = "class" =: "btn btn-outline-secondary" <> "type" =: "button"
    (btnElV,_) <- elAttr' "button" btnAttr $ text "Add V"
    (btnElL,_) <- elAttr' "button" btnAttr $ text "Add L"
    (btnElQ,_) <- elAttr' "button" btnAttr $ text "Add Q"
    let 
      newV = Endo <$> (const (V (Just "")) <$ domEvent Click btnElV)
      newL = Endo <$> (const (L (Just [])) <$ domEvent Click btnElL)
    dynB <- holdDyn False $ leftmost [True <$ domEvent Click btnElQ, False <$ newV, False <$ newL, False <$ evQ]       
    let dynEv = (\b -> if b then updated <$> namedTemplatesDropdown dbTemplates else return never) <$> dynB
    evEv <- dyn dynEv
    evQ <- switchHold never evEv
    let newQ = Endo <$> (const <$> evQ)
    return $ leftmost [newV, newL, newQ]

showNamedTemplateWidget :: MonadWidget t m => Dynamic t (M.Map NamedTemplate T.Text) -> m ()
showNamedTemplateWidget dbTemplates = divClass "container" $ do
  elClass "h2" "text-center mt-2" $ text "NamedTemplate Display"
  rowWrapper $ do
    templDyn <- namedTemplatesDropdown dbTemplates
    templDyn' <- ignoreNewIfSameAsOld (V (Just "")) templDyn
    delimiter
    dyn_ $ templateWidget'' dbTemplates <$> templDyn' 


rootWidgetWS :: MonadWidget t m => m ()
rootWidgetWS = do
  ws <- myWebSocket wSURL def
  let 
--    mEv = decodeStrict <$> (_webSocket_recv ws) 
    mEv' = decodeUtf8 <$> (_webSocket_recv ws)
    toTemps :: T.Text -> [NamedTemplate]
    toTemps = read . T.unpack    
    mListEv = toTemps <$> mEv' -- (maybe [] id) <$> mEv
    ascListEv = map (\t -> (t, templateName t)) <$> mListEv
    mapEv = M.fromAscList <$> ascListEv
  dbTemplatesDyn <- holdDyn M.empty mapEv
  tabDisplay "list-group list-group-horizontal-md" "list-group-item" $
    M.fromAscList [("tab1", ("creation", rootWidget dbTemplatesDyn)), ("tab2", ("display", showNamedTemplateWidget dbTemplatesDyn))]

myWebSocket :: (MonadJSM m, MonadJSM (Performable m), HasJSContext m, PerformEvent t m, TriggerEvent t m, PostBuild t m) => T.Text -> WebSocketConfig t ByteString -> m (WebSocket t)
myWebSocket = webSocket

ignoreNewIfSameAsOld :: (Reflex t, MonadHold t m, MonadFix m, Eq a) => a -> Dynamic t a -> m (Dynamic t a)
ignoreNewIfSameAsOld init dynVal = foldDynMaybe (\n o -> if n == o then Nothing else Just n) init (updated dynVal)


rootWidget :: MonadWidget t m => Dynamic t (M.Map NamedTemplate T.Text) -> m ()
rootWidget dbTemplatesDyn = divClass "container" $ do
        elClass "h2" "text-center mt-2" $ text "Templates"
        newTemplateEv <- createButtons dbTemplatesDyn 
        rec 
          templateDyn <- foldDyn (<>) mempty $ leftmost [newTemplateEv, editsEv, totTemEv]
          let templateDyn' = (flip appEndo) (V Nothing) <$> templateDyn 
          delimiter
          editsEv' <- dyn $ templateWidget'' dbTemplatesDyn <$> templateDyn' 
          editsEv <- switchHold never editsEv'
          delimiter
          totTemEv <- rowWrapper $ el "form" $
            divClass "input-group" $ mdo
              iEl <- inputElement $ def & initialAttributes .~ ("type" =: "text" <> "class" =: "form-control" <> "placeholder" =: "Template Name") 
                & inputElementConfig_setValue .~ (leftmost [respTextEv, noSendEv])
              let 
                templDynMaybe = toITemplate <$> templateDyn'
                namedPreTemplateDyn = (NamedTemplate <$> value iEl) <*> (constDyn Editable) 
                namedTemplateMaybe = ((\f -> fmap f) <$> namedPreTemplateDyn) <*> templDynMaybe
                filterOut :: Maybe NamedTemplate -> [NamedTemplate] -> Maybe [NamedTemplate]
                filterOut Nothing mTs = Nothing
                filterOut (Just nT) mTs = Just $ nT : mTs 
                sendEvent = attachWithMaybe filterOut (current namedTemplateMaybe) ([] <$ btnEv)
                noSendEv = difference ("Template is not ready" <$ btnEv) sendEvent
                btnAttr = "class" =: "btn btn-outline-secondary" <> "type" =: "button"
              (btnEl,_) <- divClass "input-group-append" $ elAttr' "button" btnAttr $ text "Save Template"
              let btnEv = domEvent Click btnEl
              let reqFunc = postJson  url 
              respEv <- performRequestAsync $ reqFunc <$> sendEvent 
              let respTextEv = view . _xhrResponse_responseText <$> respEv
              (btnEl',_) <- divClass "input-group-appen" $ elAttr' "button" btnAttr $ text "Clear all"
              let 
                btnEv' = domEvent Click btnEl'
                btnEndoEv = (Endo $ const (V Nothing)) <$ btnEv'
              return btnEndoEv 
          delimiter
        blank


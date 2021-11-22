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
import Common 
import Data.Aeson (decodeStrict)
import Data.ByteString (ByteString)

import Language.Javascript.JSaddle.Types
import Reflex.PerformEvent.Class

main :: IO ()
main = mainWidgetWithHead headWidget rootWidgetWS --rootWidget
--main = mainWidgetWithCss css rootWidget where
--  css = $(embedFile "css/tab.css") 

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
                divClass "col-6" x

delimiter :: MonadWidget t m => m ()
delimiter = rowWrapper $
                divClass "border-top mt-3" blank

templateWidget' :: MonadWidget t m => Dynamic t (M.Map NamedTemplate T.Text) -> Int -> Dynamic t Template -> m (Event t (Endo Templates))
templateWidget' namedTemplatesDyn  i dynTemplate = divClass "d-flex border-bottom" $ do
                          divClass "p-2 flex-grow-1 my-auto" $ 
                            dynText $ templateText <$> dynTemplate
                          divClass "p-2 my-auto" $ display $ templateType <$> dynTemplate
                          dropEv <- divClass "p-2 my-auto" $ showNamedTemplates namedTemplatesDyn
                          let dropEndoEv = Endo <$> (mapTemplateWithKey i <$> (const . template <$> dropEv))
                          divClass "p-2 btn-group"$ do 
                            let btnAttr = "class" =: "btn btn-outline-secondary" <> "type" =: "button"
                            (btnEl,_) <- elAttr' "button" btnAttr $ text "Reset"
                            (btnEl2,_) <- elAttr' "button" btnAttr $ text "Remove"
                            let 
                              btnEv = domEvent Click btnEl
                              btnEv2 = domEvent Click btnEl2
                              btnEndoEv = const (Endo $ mapTemplateWithKey i resetTemplate) <$> btnEv
                              btnEndoEv2 = const (Endo $ IM.filterWithKey (\k _ -> k /= i)) <$> btnEv2
                            return $ leftmost [btnEndoEv, btnEndoEv2, dropEndoEv] 


templateListWidget' :: MonadWidget t m => Dynamic t (M.Map NamedTemplate T.Text) -> Dynamic t Templates -> m(Event t (Endo Templates))
templateListWidget' dbTemplatesDyn templatesDyn = rowWrapper $ do 
        x <- listWithKey (M.fromAscList . IM.toAscList <$> templatesDyn) $ templateWidget' dbTemplatesDyn
        divClass "p-2 btn-group"$ do 
          let btnAttr = "class" =: "btn btn-outline-secondary" <> "type" =: "button"
          (btnEl,_) <- elAttr' "button" btnAttr $ text "Clear all"
          let 
            btnEv = domEvent Click btnEl
            btnEndoEv = const (Endo $ const mempty) <$> btnEv
          (btnEl2,_) <- elAttr' "button" btnAttr $ text "Save all"
          let allEv = btnEndoEv --leftmost [btnEndoEv, dropEndoEv] 
          return $ leftmost [allEv,  switchDyn (leftmost . M.elems <$> x)]
        

newTemplateForm' :: MonadWidget t m => m (Event t (Endo Templates))
newTemplateForm' = rowWrapper $ el "form" $
                divClass "input-group" $ mdo
                  iEl <- inputElement $ def & initialAttributes .~ ("type" =: "text" <> "class" =: "form-control" <> "placeholder" =: "Template'") 
                    & inputElementConfig_setValue .~ ("" <$ btnEv)
                  let 
                    newTemplateDyn = newTemplate <$> value iEl
                    btnAttr = "class" =: "btn btn-outline-secondary" <> "type" =: "button"
                  (btnEl,_) <- divClass "input-group-append" $ elAttr' "button" btnAttr $ text "Add new entry"
                  let btnEv = domEvent Click btnEl
                  return $ (\template -> Endo $ addTemplate template) <$> (tagPromptlyDyn newTemplateDyn $ btnEv)

rootWidget' :: MonadWidget t m => Dynamic t (M.Map NamedTemplate T.Text) -> m ()
rootWidget' dbTemplatesDyn = divClass "container" $ do
        elClass "h2" "text-center mt-2" $ text "Templates"
        newTemplateEv <- newTemplateForm'
        rec 
          templateDyn <- foldDyn (<>) mempty $ leftmost [newTemplateEv, editsEv, totTemEv]
          let templatesDyn = (flip appEndo) mempty <$> templateDyn --Dynamic t (IM.IntMap Template)
          delimiter
          editsEv <- templateListWidget' dbTemplatesDyn templatesDyn 
          delimiter
          totTemEv <- rowWrapper $ el "form" $
            divClass "input-group" $ mdo
              iEl <- inputElement $ def & initialAttributes .~ ("type" =: "text" <> "class" =: "form-control" <> "placeholder" =: "Template Name") 
                & inputElementConfig_setValue .~ (respTextEv)
              let 
                namedTemplateDyn = (NamedTemplate <$> value iEl) <*> (constDyn Editable) <*> (L . IM.elems <$> templatesDyn)
                namedTemplatesDyn = (flip (:)) [] <$> namedTemplateDyn
                btnAttr = "class" =: "btn btn-outline-secondary" <> "type" =: "button"
              (btnEl,_) <- divClass "input-group-append" $ elAttr' "button" btnAttr $ text "Save Template"
              let btnEv = domEvent Click btnEl
              let reqFunc = postJson  url 
              respEv <- performRequestAsync $ reqFunc <$> (tag (current namedTemplatesDyn) $ btnEv)
              let respTextEv = view . _xhrResponse_responseText <$> respEv
              --asText <- holdDyn "No result" respTextEv
              --dynText asText
              (btnEl',_) <- divClass "input-group-appen" $ elAttr' "button" btnAttr $ text "Clear all"
              let 
                btnEv' = domEvent Click btnEl'
                btnEndoEv = const (Endo $ const mempty) <$> btnEv'
              return btnEndoEv 
          delimiter
--          dbTemplatesDyn <- divClass "p-2 btn-group"$ do 
--            let btnAttr = "class" =: "btn btn-outline-secondary" <> "type" =: "button"
--            (btnEl2,_) <- elAttr' "button" btnAttr $ text "LoadDB"
--            mEv <- getAndDecode (const url <$> domEvent Click btnEl2)
--            let 
--              mListEv = (maybe [] id) <$> mEv
--              ascListEv = map (\t -> (t, templateName t)) <$> mListEv
--              mapEv = M.fromAscList <$> ascListEv
--            holdDyn mempty mapEv 
                
        blank

--handleRequest :: Event t a -> m Event t Text
url = "http://localhost:8081/templates" :: T.Text

view :: Maybe T.Text -> T.Text
view mText = maybe "FAILED" id mText

--getFromDB urlEv = do
--    mEv <- getAndDecode urlEv 
--    let 
--      mListEv = (maybe [] id) <$> mEv
--      ascListEv = map (\t -> (t, templateText t)) <$> mListEv
--      mapEv = M.fromAscList <$> ascListEv
--    holdDyn mempty mapEv 
--handleRequest evVal = do
--    respEv <- performRequestAsync evVal
--    return $ view . _xhrResponse_responseText <$> respEv

namedTemplatesWidget :: MonadWidget t m => Dynamic t (M.Map NamedTemplate T.Text) -> m (Event t NamedTemplate)
namedTemplatesWidget ntemplatesDyn = do 
    d <- dropdown (NamedTemplate "---select---" Done (V "")) ntemplatesDyn def
    return $ _dropdown_change d
    
namedTemplatesDropdown :: MonadWidget t m => Dynamic t (M.Map NamedTemplate T.Text) -> m (Dynamic t Template)
namedTemplatesDropdown ntemplatesDyn = do 
    d <- dropdown (NamedTemplate "---select---" Done (V "")) ntemplatesDyn def
    return $ template <$> _dropdown_value d

showNamedTemplates :: MonadWidget t m => Dynamic t (M.Map NamedTemplate T.Text)-> m (Event t NamedTemplate)
showNamedTemplates ntemplatesDyn = el "form" $ do
    cBox <- divClass "col-3" $ inputElement $ def
        & inputElementConfig_initialChecked .~ False
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "checkbox")
    let dV = _inputElement_checked cBox
    divClass "col-3" $ do
      evEv <- dyn $ (\b -> if b then namedTemplatesWidget ntemplatesDyn else return never) <$> dV
      switchHold never evEv
      
showTemplate :: MonadWidget t m => Template -> m ()
showTemplate (V s) = (dynText $ constDyn $ T.pack s) >> delimiter
showTemplate (L ts) = do
    cBox <- divClass "col-3" $ inputElement $ def
        & inputElementConfig_initialChecked .~ False
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "checkbox")
    let dV = _inputElement_checked cBox
    divClass "col-3" $ do
      dyn $ (\b -> if b then foldl (>>) (return ()) (map showTemplate ts)  else return () ) <$> dV
      blank

rootWidget :: MonadWidget t m => m ()
rootWidget = do
  dbTemplatesDyn <- divClass "p-2 btn-group"$ do 
    let btnAttr = "class" =: "btn btn-outline-secondary" <> "type" =: "button"
    (btnEl2,_) <- elAttr' "button" btnAttr $ text "LoadDB"
    mEv <- getAndDecode (const url <$> domEvent Click btnEl2)
    let 
      mListEv = (maybe [] id) <$> mEv
      ascListEv = map (\t -> (t, templateName t)) <$> mListEv
      mapEv = M.fromAscList <$> ascListEv
    holdDyn mempty mapEv 
  tabDisplay "list-group list-group-horizontal-md" "list-group-item" $
    M.fromAscList [("tab1", ("creation", rootWidget' dbTemplatesDyn)), ("tab2", ("display", showNamedTemplateWidget dbTemplatesDyn))]

showNamedTemplateWidget :: MonadWidget t m => Dynamic t (M.Map NamedTemplate T.Text) -> m ()
showNamedTemplateWidget dbTemplates = divClass "container" $ do
  elClass "h2" "text-center mt-2" $ text "NamedTemplate Display"
  rowWrapper $ do
    templDyn <- namedTemplatesDropdown dbTemplates
    templDyn' <- foldDynMaybe (\nVal oVal -> if nVal == oVal then Nothing else Just nVal) (V "") (updated templDyn)
    delimiter
    dyn_ $ showTemplate <$> templDyn'


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
--  respText <- holdDyn "Nothing yet" mEv'
--  dynText respText
  dbTemplatesDyn <- holdDyn M.empty mapEv
  tabDisplay "list-group list-group-horizontal-md" "list-group-item" $
    M.fromAscList [("tab1", ("creation", rootWidget' dbTemplatesDyn)), ("tab2", ("display", showNamedTemplateWidget dbTemplatesDyn))]

myWebSocket :: (MonadJSM m, MonadJSM (Performable m), HasJSContext m, PerformEvent t m, TriggerEvent t m, PostBuild t m) => T.Text -> WebSocketConfig t ByteString -> m (WebSocket t)
myWebSocket = webSocket


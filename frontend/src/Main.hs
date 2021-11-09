{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
module Main where

import Reflex.Dom
import qualified Data.Text as T 
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Monoid (Endo(..), appEndo)
import Control.Monad (void)
import Common 

main :: IO ()
main = mainWidgetWithHead headWidget rootWidget'

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
  el "title" $ text "TODO App"

rowWrapper :: MonadWidget t m => m a -> m a
rowWrapper x = divClass "row justify-content-md-center" $
                divClass "col-6" x

delimiter :: MonadWidget t m => m ()
delimiter = rowWrapper $
                divClass "border-top mt-3" blank

templateWidget' :: MonadWidget t m => Int -> Dynamic t Template -> m (Event t (Endo Templates))
templateWidget' i dynTemplate = divClass "d-flex border-bottom" $ do
                          divClass "p-2 flex-grow-1 my-auto" $ 
                            dynText $ templateText <$> dynTemplate
                          divClass "p-2 my-auto" $ display $ templateStatus <$> dynTemplate
                          divClass "p-2 my-auto" $ showNamedTemplates namedTemplates
                          divClass "p-2 btn-group"$ do 
                            let btnAttr = "class" =: "btn btn-outline-secondary" <> "type" =: "button"
                            (btnEl,_) <- elAttr' "button" btnAttr $ text "Fix Status"
                            let btnEv = domEvent Click btnEl
                            (btnEl2,_) <- elAttr' "button" btnAttr $ text "Empty"
                            return $ const (Endo $ mapTemplateWithKey i setStatusToDone) <$> btnEv 


templateListWidget' :: MonadWidget t m => Dynamic t Templates -> m(Event t (Endo Templates))
templateListWidget' templatesDyn = rowWrapper $ do 
        x <- listWithKey (M.fromAscList . IM.toAscList <$> templatesDyn) templateWidget'
        return $ switchDyn (leftmost . M.elems <$> x)
        

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


rootWidget' :: MonadWidget t m => m ()
rootWidget' = divClass "container" $ do
        elClass "h2" "text-center mt-2" $ text "Templates"
        newTemplateEv <- newTemplateForm'
        rec 
          templateDyn <- foldDyn (<>) mempty $ leftmost [newTemplateEv, editsEv]
          let templatesDyn = (flip appEndo) mempty <$> templateDyn --Dynamic t (IM.IntMap Template)
          delimiter
          editsEv <- templateListWidget' templatesDyn 
        blank

namedTemplatesWidget :: MonadWidget t m => M.Map Template T.Text -> m ()
namedTemplatesWidget ntemplates = do 
    d <- dropdown (Template "---select---" Done) (constDyn ntemplates) def
    display $ _dropdown_value d

showNamedTemplates :: MonadWidget t m => M.Map Template T.Text-> m ()
showNamedTemplates ntemplates = do
    cBox <- inputElement $ def
        & inputElementConfig_initialChecked .~ False
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "checkbox")
    let dV = _inputElement_checked cBox
    delimiter
    dyn_ $ (\b -> if b then namedTemplatesWidget ntemplates else blank) <$> dV
    

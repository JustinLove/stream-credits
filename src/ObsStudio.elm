port module ObsStudio exposing (onVisibilityChange)

import Browser.Events

onVisibilityChange : (Browser.Events.Visibility -> msg) -> Sub msg
onVisibilityChange tagger =
  obsStudioOnVisibilityChange (encodeVisible >> tagger)

encodeVisible : Bool -> Browser.Events.Visibility
encodeVisible visible =
  if visible then
    Browser.Events.Visible
  else
    Browser.Events.Hidden

port obsStudioOnVisibilityChange : (Bool -> msg) -> Sub msg

---
date: 2024-01-20 17:44:34.571117-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902?) Elm \u092E\
  \u0947\u0902 \u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\
  \u094B\u0921 \u0915\u0930\u0928\u093E http \u0915\u093E\u0930\u094D\u092F\u094B\u0902\
  \ \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 \u0915\u093F\
  \u092F\u093E \u091C\u093E\u0924\u093E \u0939\u0948\u0964 \u0928\u0940\u091A\u0947\
  \ \u090F\u0915 \u0938\u093E\u0927\u093E\u0930\u0923 \u0915\u094B\u0921 \u0915\u093E\
  \ \u0909\u0926\u093E\u0939\u0930\u0923 \u0926\u093F\u092F\u093E \u0917\u092F\u093E\
  \ \u0939\u0948."
lastmod: '2024-04-05T21:53:54.192412-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902?) Elm \u092E\u0947\u0902\
  \ \u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\u0921\
  \ \u0915\u0930\u0928\u093E http \u0915\u093E\u0930\u094D\u092F\u094B\u0902 \u0915\
  \u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 \u0915\u093F\u092F\
  \u093E \u091C\u093E\u0924\u093E \u0939\u0948\u0964 \u0928\u0940\u091A\u0947 \u090F\
  \u0915 \u0938\u093E\u0927\u093E\u0930\u0923 \u0915\u094B\u0921 \u0915\u093E \u0909\
  \u0926\u093E\u0939\u0930\u0923 \u0926\u093F\u092F\u093E \u0917\u092F\u093E \u0939\
  \u0948."
title: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E"
weight: 42
---

## How to: (कैसे करें?)
Elm में वेब पेज डाउनलोड करना http कार्यों का उपयोग करके किया जाता है। नीचे एक साधारण कोड का उदाहरण दिया गया है:

```Elm
module Main exposing (..)

import Http
import Html exposing (Html, text)
import Json.Decode as Decode

type Msg 
    = GotText (Result Http.Error String)

type alias Model =
    { content : String }

initialModel : Model
initialModel =
    { content = "" }

update : Msg -> Model -> Model
update msg model =
    case msg of
        GotText (Ok text) ->
            { model | content = text }

        GotText (Err _) ->
            model -- यहाँ त्रुटि हैंडल करें

view : Model -> Html Msg
view model =
    Html.div []
        [ Html.text model.content ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

main =
    Html.program
        { init = (initialModel, downloadPage "http://example.com")
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

downloadPage : String -> Cmd Msg
downloadPage url =
    Http.getString url
        |> Http.send GotText
```

जब आप इस कोड को रन करेंगे, Elm `http://example.com` से पेज का टेक्स्ट डाउनलोड करेगा और `model.content` में स्टोर कर देगा।

## Deep Dive (गहन जानकारी)
Elm में वेब पेज डाउनलोड करने की पद्धति अपेक्षाकृत नई है। पुराने संस्करण में इसके लिए बहुत सारी कस्टम जावास्क्रिप्ट लिखनी पड़ती थी, लेकिन Elm 0.19 में `Http` मॉड्यूल के साथ यह सरल हो गया है। `Http.getString` एक आसान फंक्शन है जो GET रिक्वेस्ट करके टेक्स्ट के रूप में डेटा लौटाता है।

विकल्प के तौर पर, आप `Http.get` या `Http.request` का उपयोग कर सकते हैं जब आपको अधिक जटिल अनुरोध करना होता है, जैसे जेसन डेटा प्राप्त करना या हेडर्स सेट करना।

Elm में एक महत्वपूर्ण बात यह है कि सभी साइड इफ़ेक्ट्स (जैसे कि HTTP रिक्वेस्ट्स) कमांड्स (Cmd) के माध्यम से होते हैं। यह प्योर फंक्शनल प्रोग्रामिंग का पालन करते हुए आपके एप्लीकेशन को प्रेडिक्टेबल बनाता है।

## See Also (और भी देखें)
- [Elm HTTP package documentation](https://package.elm-lang.org/packages/elm/http/latest/)
- [Elm guide on HTTP](https://guide.elm-lang.org/effects/http.html)
- [JSON decoding in Elm](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode)

---
aliases:
- /hi/elm/downloading-a-web-page/
date: 2024-01-20 17:44:34.571117-07:00
description: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\
  \u094B\u0921 \u0915\u0930\u0928\u093E \u092E\u0924\u0932\u092C \u0907\u0902\u091F\
  \u0930\u0928\u0947\u091F \u0938\u0947 \u0938\u0942\u091A\u0928\u093E \u0905\u092A\
  \u0928\u0947 \u090F\u092A\u094D\u0932\u0940\u0915\u0947\u0936\u0928 \u092E\u0947\
  \u0902 \u0932\u093E\u0928\u093E\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\
  \u093E\u092E\u0930 \u0907\u0938\u0915\u094B \u0915\u0930\u0924\u0947 \u0939\u0948\
  \u0902 \u0921\u0947\u091F\u093E \u092A\u094D\u0930\u093E\u092A\u094D\u0924 \u0915\
  \u0930\u0928\u0947, \u0938\u0902\u0938\u093E\u0927\u093F\u0924 \u0915\u0930\u0928\
  \u0947 \u0914\u0930 \u0909\u092A\u092F\u094B\u0917\u0915\u0930\u094D\u0924\u093E\
  \u0913\u0902 \u0915\u094B \u0926\u093F\u0916\u093E\u0928\u0947 \u0915\u0947\u2026"
lastmod: 2024-02-18 23:09:03.189709
model: gpt-4-1106-preview
summary: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E \u092E\u0924\u0932\u092C \u0907\u0902\u091F\u0930\
  \u0928\u0947\u091F \u0938\u0947 \u0938\u0942\u091A\u0928\u093E \u0905\u092A\u0928\
  \u0947 \u090F\u092A\u094D\u0932\u0940\u0915\u0947\u0936\u0928 \u092E\u0947\u0902\
  \ \u0932\u093E\u0928\u093E\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\
  \u092E\u0930 \u0907\u0938\u0915\u094B \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\
  \ \u0921\u0947\u091F\u093E \u092A\u094D\u0930\u093E\u092A\u094D\u0924 \u0915\u0930\
  \u0928\u0947, \u0938\u0902\u0938\u093E\u0927\u093F\u0924 \u0915\u0930\u0928\u0947\
  \ \u0914\u0930 \u0909\u092A\u092F\u094B\u0917\u0915\u0930\u094D\u0924\u093E\u0913\
  \u0902 \u0915\u094B \u0926\u093F\u0916\u093E\u0928\u0947 \u0915\u0947\u2026"
title: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

वेब पेज डाउनलोड करना मतलब इंटरनेट से सूचना अपने एप्लीकेशन में लाना। प्रोग्रामर इसको करते हैं डेटा प्राप्त करने, संसाधित करने और उपयोगकर्ताओं को दिखाने के लिए।

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

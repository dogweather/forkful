---
title:                "वेब पेज डाउनलोड करना"
aliases: - /hi/elm/downloading-a-web-page.md
date:                  2024-01-20T17:44:34.571117-07:00
model:                 gpt-4-1106-preview
simple_title:         "वेब पेज डाउनलोड करना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/downloading-a-web-page.md"
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

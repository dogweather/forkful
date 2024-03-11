---
date: 2024-01-20 17:54:28.015446-07:00
description: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\u0907\u0932\
  \ \u092A\u0922\u093C\u0928\u093E \u092B\u093C\u093E\u0907\u0932 \u0938\u0947 \u0921\
  \u0947\u091F\u093E \u090F\u0915\u094D\u0938\u0947\u0938 \u0915\u0930\u0928\u0947\
  \ \u0915\u093E \u090F\u0915 \u0924\u0930\u0940\u0915\u093E \u0939\u0948\u0964 \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0921\u0947\u091F\
  \u093E \u092A\u094D\u0930\u094B\u0938\u0947\u0938\u093F\u0902\u0917, \u0915\u0949\
  \u0928\u094D\u092B\u093F\u0917 \u0938\u0947\u091F\u093F\u0902\u0917\u094D\u0938\
  , \u092F\u093E \u0932\u0949\u0917\u094D\u0938 \u0915\u093E \u090F\u0928\u093E\u0932\
  \u093F\u0938\u093F\u0938 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F\
  \ \u092F\u0939 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\u0964"
lastmod: '2024-03-11T00:14:26.118383-06:00'
model: gpt-4-1106-preview
summary: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\u0907\u0932\
  \ \u092A\u0922\u093C\u0928\u093E \u092B\u093C\u093E\u0907\u0932 \u0938\u0947 \u0921\
  \u0947\u091F\u093E \u090F\u0915\u094D\u0938\u0947\u0938 \u0915\u0930\u0928\u0947\
  \ \u0915\u093E \u090F\u0915 \u0924\u0930\u0940\u0915\u093E \u0939\u0948\u0964 \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0921\u0947\u091F\
  \u093E \u092A\u094D\u0930\u094B\u0938\u0947\u0938\u093F\u0902\u0917, \u0915\u0949\
  \u0928\u094D\u092B\u093F\u0917 \u0938\u0947\u091F\u093F\u0902\u0917\u094D\u0938\
  , \u092F\u093E \u0932\u0949\u0917\u094D\u0938 \u0915\u093E \u090F\u0928\u093E\u0932\
  \u093F\u0938\u093F\u0938 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F\
  \ \u092F\u0939 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\u0964"
title: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\u0907\u0932\
  \ \u092A\u0922\u093C\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
टेक्स्ट फ़ाइल पढ़ना फ़ाइल से डेटा एक्सेस करने का एक तरीका है। प्रोग्रामर्स डेटा प्रोसेसिंग, कॉन्फिग सेटिंग्स, या लॉग्स का एनालिसिस करने के लिए यह करते हैं।

## How to: (कैसे करें:)
Elm में सीधे तरीके से फाइल पढ़ना संभव नहीं है क्योंकि यह ब्राउज़र पर चलता है और फ़ाइल सिस्टम तक एक्सेस नहीं होता। लेकिन, हम यूजर से एक फाइल अपलोड करने के लिए कह सकते हैं और फिर Elm उसे पढ़ सकता है। यहां एक उदाहरण है:

```Elm
module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import File exposing (File)
import File.Select as FileSelect

type Msg
    = SelectFile
    | FileSelected (Maybe File)
    | ReadFile
    | FileReaderResult (Result () String)

type alias Model =
    { file : Maybe File
    , fileContent : Maybe String
    }

initialModel : Model
initialModel =
    { file = Nothing
    , fileContent = Nothing
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SelectFile ->
            (model, FileSelect.file [] FileSelected)

        FileSelected maybeFile ->
            ({ model | file = maybeFile }, Cmd.none)

        ReadFile ->
            case model.file of
                Just file ->
                    (model, File.readAsText file FileReaderResult)

                Nothing ->
                    (model, Cmd.none)

        FileReaderResult (Ok content) ->
            ({ model | fileContent = Just content }, Cmd.none)

        FileReaderResult (Err _) ->
            (model, Cmd.none)

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SelectFile ] [ text "Select a file" ]
        , case model.fileContent of
              Just content ->
                  div [] [ text content ]

              Nothing ->
                  text ""
        ]

main =
    Browser.element
        { init = \_ -> (initialModel, Cmd.none)
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
```

ऊपर दिया कोड यूजर से एक फाइल चुनने और उसे पढ़ने की कार्यवाही करता है।

## Deep Dive (गहराई में जानकारी)
Elm में फ़ाइल पढ़ना ब्राउज़र सुरक्षा प्रतिबन्धों की वजह से एक सीमित ऑपरेशन है। Elm को डिज़ाइन किया गया था ताकि यूजर की सुरक्षा और उसके डाटा प्रीवेसी को बनाए रख सकें। इसके अलावा, Elm में साइड-इफेक्ट्स (जैसेकि IO ऑपरेशन्स) को हैंडल करने का एक स्ट्रक्चर्ड तरीका है, इसलिए फ़ाइल पढ़ने के लिए विशेष इवेंट्स और मॉड्यूल्स का इस्तेमाल किया जाता है। जैसा कि उदाहरण में देखा जा सकता है, `File.readAsText` का इस्तेमाल करते हुए `FileReaderResult` में काम्य/असफल पढ़ाई का नतीजा मिलता है।

## See Also (संबंधित सूत्र)
- Elm `File` module documentation: [Elm File Module](https://package.elm-lang.org/packages/elm/file/latest/)
- Elm Language Guide: [Elm Guide](https://guide.elm-lang.org/)
- Elm File Reader example: [Elm File Reader Example](https://ellie-app.com/new)
- MDN FileReader API Documentation: [MDN FileReader API](https://developer.mozilla.org/en-US/docs/Web/API/FileReader)

---
title:                "टेक्स्ट फ़ाइल पढ़ना"
date:                  2024-01-20T17:54:28.015446-07:00
model:                 gpt-4-1106-preview
simple_title:         "टेक्स्ट फ़ाइल पढ़ना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/reading-a-text-file.md"
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
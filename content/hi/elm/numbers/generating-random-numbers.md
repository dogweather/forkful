---
title:                "यादृच्छिक संख्याओं का उत्पन्न करना"
date:                  2024-02-27T22:50:50.417673-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-02-27, dogweather, edited and tested
  - 2024-02-27, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
Elm में यादृच्छिक संख्याओं का उत्पादन `Random` मोड्यूल का उपयोग करते हुए किया जाता है जो प्रीडो-रैंडम संख्याएं प्रदान करता है, जो खेलों, सिमुलेशनों और यहां तक कि ऐसे एलगोरिथमों के लिए भी उपयोगी होता है जिन्हें स्थोकीला प्रक्रियाओं की आवश्यकता होती है। यह क्षमता डेवलपर्स को उनके अनुप्रयोगों में अनिश्चितता और विविधता जोड़ने में सक्षम बनाती है, जिससे उपयोगकर्ता अनुभव और फंक्शनैलिटी में सुधार होता है।

## कैसे करें:
Elm की शुद्ध फंक्शनल प्रकृति का मतलब है कि आप सीधे यादृच्छिक संख्याएं उत्पन्न नहीं कर सकते जैसा कि आप आज्ञाकारी भाषाओं में कर सकते हैं। इसके बजाय, आप `Random` मॉड्यूल को कमांड्स के संग उपयोग करते हैं। यहां एक मूल उदाहरण दिया गया है जो 1 से 100 के बीच एक यादृच्छिक पूर्णांक उत्पन्न करता है।

सबसे पहले, `elm install elm/random` के साथ `Random` मोड्यूल स्थापित करें। फिर इसे आपकी Elm फ़ाइल में आवश्यक HTML और इवेंट मॉड्यूलों के साथ इम्पोर्ट करें, ऐसे:

`src/Main.elm`

```elm
module Main exposing (..)

import Browser
import Html exposing (Html, button, text, div)
import Html.Events exposing (onClick)
import Random
```

इसे एक आत्म-निहित उदाहरण बनाने के लिए, आप इस बॉयलरप्लेट को जोड़ सकते हैं:
```elm
main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }

init : () -> (Model, Cmd Msg)
init _ =
  (Model 0, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
```

आगे, एक **कमांड** परिभाषित करें जो एक यादृच्छिक संख्या उत्पन्न करता है। इसमें एक `Msg` प्रकार की स्थापना शामिल है जो यादृच्छिक संख्या को संभालता है जब वह उत्पन्न होती है, एक `Model` जो इसे संग्रहीत करता है, और एक अपडेट फ़ंक्शन जो इसे सब एक साथ बांधता है।
```elm
type Msg
    = Generate
    | NewRandom Int

type alias Model = { randomNumber : Int }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Generate ->
            ( model, Random.generate NewRandom (Random.int 1 100) )

        NewRandom number ->
            ( { model | randomNumber = number }, Cmd.none )
```

एक संख्या उत्पन्न करने के लिए ट्रिगर करने हेतु, आप एक `Generate` संदेश भेजेंगे, उदाहरण के लिए, अपने दृश्य में एक बटन के माध्यम से:
```elm
view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("Random Number: " ++ String.fromInt model.randomNumber) ]
        , button [ onClick Generate ] [ text "Generate" ]
        ]
```

जब आप "Generate" बटन पर क्लिक करेंगे, तब 1 से 100 के बीच एक यादृच्छिक संख्या प्रदर्शित होगी।

यह सरल दृष्टिकोण अनुकूलित और विस्तारित किया जा सकता है, `Random` मोड्यूल में अन्य फंक्शनों का लाभ उठाकर यादृच्छिक फ्लोट्स, सूचियां, या यहां तक कि कस्टम प्रकारों पर आधारित जटिल डेटा संरचनाएं उत्पन्न की जा सकती हैं, जो आपके Elm अनुप्रयोगों में अनिश्चितता जोड़ने के लिए विशाल खेल का मैदान प्रदान करती हैं।

Elm गाइड बहुत अधिक विस्तार में जाती है। इसमें [एक छह पक्षीय डाइ को रोल करने का उदाहरण भी है](https://guide.elm-lang.org/effects/random).

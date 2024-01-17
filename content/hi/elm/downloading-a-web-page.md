---
title:                "वेब पेज डाउनलोड करना"
html_title:           "Elm: वेब पेज डाउनलोड करना"
simple_title:         "वेब पेज डाउनलोड करना"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्या और क्यों?:

वेब पृष्ठ डाउनलोड क्या है और इसे क्यों प्रोग्रामर्स करते हैं? वेब पृष्ठ डाउनलोड से तात्पर्य है कि हम एक वेब पृष्ठ को अपने स्थानीय मशीन पर संग्रहीत करते हैं। यह प्रोग्रामर्स को विभिन्न उदाहरणों और अवसरों में उनके कूद को प्रदर्शित करने में मदद करता है।

## कैसे करें:

```Elm
import Html
import Http

type Msg = Fetched (Result Http.Error String)

getWebPage : Cmd Msg
getWebPage =
  let
    request =
      Http.getString "https://www.example.com"
  in
    Http.send Fetched request

view : Model -> Html Msg
view model =
  Html.button
    [ onClick model.getWebPage ]
    [ Html.text "Get the Web Page!" ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

main : Program Never
main =
  Html.beginnerProgram
    { model = initialModel
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
```

निष्कर्ष:
"Get the Web Page!" बटन पर क्लिक करें, इससे एक HTTP GET अनुरोध भेजा जाएगा और वेब पृष्ठ का सामग्री मॉडल में संग्रहीत होगी।

## गहराई तक:

वेब पृष्ठ डाउनलोड को समझने के लिए इतिहास, वैकल्पिक विकल्प और प्रगति की विवरण शामिल होते हैं। चरणों को कई भाषाओं में संसाधित किया गया है और हम डाउनलोड के साथ ही अन्य वेब घटकों को भी प्राप्त कर सकते हैं। Elm, प्रोग्रामिंग भाषा है जो फ़ंक्शनल और लक्षित समझ का उपयोग करती है और जो भारतवर्ष के भाषाओं को पूर्णतः समर्थित करती है।

## जरूर देखें:

- Elm वेबसाइट: https://elm-lang.org/
- Elm वेबसाइट के कोड: https://github.com/elm/elm-website
- Elm वेब पृष्ठ डाउनलोड के लिए Http आईओ आइडेंटिटी: https://github.com/elm/http/blob/master/src/Http.elm
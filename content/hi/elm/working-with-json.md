---
title:                "JSON के साथ काम करना"
date:                  2024-01-19
html_title:           "Arduino: JSON के साथ काम करना"
simple_title:         "JSON के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/working-with-json.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
JSON, जो JavaScript Object Notation के लिए है, एक डेटा स्वरुप है जिसका इस्तेमाल डेटा का आदान-प्रदान करने के लिए किया जाता है। प्रोग्रामर इसका उपयोग APIs से डेटा प्राप्त करने या भेजने हेतु करते हैं।

## कैसे करें:
अगर आपको JSON से काम करना है, तब Elm में इसे देखें:

```Elm
import Json.Decode exposing (Decoder, field, string, int)
import Http

type alias User =
    { id : Int
    , name : String
    }

userDecoder : Decoder User
userDecoder =
    Json.Decode.map2 User
        (field "id" int)
        (field "name" string)

fetchUser : Int -> Cmd Msg
fetchUser userId =
    Http.get
        { url = "https://jsonplaceholder.typicode.com/users/" ++ String.fromInt(userId)
        , expect = Http.expectJson GotUserData userDecoder
        }
```

यहां `userDecoder` एक JSON डिकोडर है जो यूजर आईडी और नाम को पढ़ने के लिए है। `fetchUser` फंग्शन API से डेटा मंगाती है।

## गहराई में:
Elm में, JSON के साथ काम करना प्रकार-सुरक्षित है। Elm 0.19 ने JSON डिकोडिंग को और आसान बनाया है। डिकोडर्स का निर्माण करके, Elm विकासक त्रुटि-मुक्त एप्लिकेशन बना सकते हैं। अलटरनेटिव्स में TypeScript और PureScript हैं, लेकिन Elm की अपनी लाइब्रेरीज और टूल्स के साथ तालमेल है।

## यह भी देखें:
- आधिकारिक Elm गाइड: [JSON Decoding](https://guide.elm-lang.org/effects/json.html)
- JSONPlaceholder (टेस्टिंग API): [JSONPlaceholder](https://jsonplaceholder.typicode.com/)
- Elm पैकेज: [elm/json](https://package.elm-lang.org/packages/elm/json/latest/)

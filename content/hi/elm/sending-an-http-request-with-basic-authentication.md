---
title:                "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
html_title:           "Elm: बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्यों

**HTTP रिक्वेस्ट** भेजना, साधारण प्रमाणीकरण के साथ, डेटा आरंभिकीकरण और अन्य प्रक्रियाओं के लिए आवश्यक हो सकता है। यह यूआरएल, बॉडी डाटा और अन्य मेटा डेटा को भेजने के लिए एक अधिक प्रभावी और सुरक्षित तरीका है।

## कैसे

आप Elm में `Http.send` और `Http.basicAuth` फ़ंक्शन का उपयोग करके स्थानिक सर्वर से डेटा डाउनलोड करने के लिए स्थानिक API से डेटा सीधे बना सकते हैं। नीचे एक उदाहरण दिया गया है:

```Elm
import Http
import Json.Decode exposing (int, string)

type alias User =
    { id : Int
    , name : String
    }

getUser : Cmd Msg
getUser =
    Http.send GotUser (Http.get "https://example.com/users/1" decodeUser)

decodeUser : Decoder User
decodeUser =
    Json.Decode.map2 User
        (Json.Decode.field "id" int)
        (Json.Decode.field "name" string)

type Msg
    = GotUser (Result Http.Error User)
```

इस उदाहरण में, हम `Http.get` का उपयोग करके एक GET रिक्वेस्ट भेजते हैं और उसे `Http.send` के द्वारा एक `Cmd` में पेश करते हैं। `decodeUser` फ़ंक्शन के आधार पर, हम सर्वर से प्राप्त डेटा को अनुकूलित करते हैं और `User` के प्रकार को प्राप्त किया जाता है। अंत में, हम `Msg` डेटा के रूप में प्राप्त होने वाले प्रतिक्रिया के साथ मैसेज के साथ `GotUser` डेटा को प्रेषित करते हैं।

## गहराई में जाएँ

HTTP रिक्वेस्ट भेजने के दौरान, साधारण प्रमाणीकरण दर्ज करने के लिए, आपको सुरक्षा और गोपनीयता समस्याओं से निपटने की जरूरत हो सकती है। इसका मतलब है कि जब आप एक HTTP अनुरोध भेजते हैं, तो आपको अपने डेटा को एक आधार पर सुरक्षित करने की
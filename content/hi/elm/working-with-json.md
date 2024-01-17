---
title:                "Json के साथ काम करना"
html_title:           "Elm: Json के साथ काम करना"
simple_title:         "Json के साथ काम करना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/working-with-json.md"
---

{{< edit_this_page >}}

## आवश्यकता क्या है और क्यों?
प्रोग्रामर्स को डेटा को संरचित रूप से स्टोर और ट्रांसफर करने के लिए जावास्क्रिप्ट ऑब्जेक्ट नोटेशन (JSON) का उपयोग करना पड़ता है। आजकल, वेब विकास में JSON का प्रयोग बहुत ही आम हो गया है। यह सुनिश्चित करता है कि डेटा एक स्ट्रक्चरल और संरचित ढंग से प्रवाहित होता है और साथ ही डेटा के साथ काम करना आसान और समझने में भी सरल होता है।

## कैसे:
जब हम कोई स्ट्रिंग या जावास्क्रिप्ट ऑब्जेक्ट को एल्म (Elm) में डेकोड करते हैं,तो वह ऑब्जेक्ट एल्म के आधिकारिक JsonObject टाइप में आता है। जोड़ों (pair) का सेट बनाने के लिए mapKeys या mapValues फंक्शन का प्रयोग किया जा सकता है।

```Elm
import Http
import Json.Decode as Decode

type alias User = {
    name : String,
    email : String
}

userDecoder : Decoder User
userDecoder =
    Decode.map2 User
        (Decode.field "name" Decode.string)
        (Decode.field "email" Decode.string)

Http.get
    { url = "https://example.com/users"
    , expect = Http.expectJson userDecoder
    }
```

Json डेकोड करते समय, हम सीधे प्रिंट आउट कर सकते हैं या डेटा को ट्रांसफर करने से पहले एल्म के ऑब्जेक्ट में कनवर्ट कर सकते हैं।

## गहराई में:
JSON (जावास्क्रिप्ट ऑब्जेक्ट नोटेशन) एक टेक्नोलॉजी है जो ओपन स्टैंडर्ड से भी तेजी से विकसित हो गई। यह आसानी से समझने में भी सरल है और इसकी उपयोगिता वृद्धि और दूसरे टेक्नोलॉजियों के साथ संयुक्त अनुप्रयोग करने की क्षमता इसे एक लोकप्रिय विकल्प बनाती है। जेसन के साथ काम करने के अलावा, खासकर एल्म (Elm) के साथ काम करने के लिए अन्य अलग विकल्प भी मौजूद हैं, जैसे कि XML, CSV आदि।

## अपनी जरुरतों के अनुसार JSON को लागू करें:
- [Elm डॉक्यूमेंटेशन] (https://guide.elm-lang.org/) एल्म की डोक्यूमेंटेशन ऑनलाइन।
- [JSON विश्लेषिकी (JSON parser)] (https://github.com/elm-community/json-extra) एल्म के लिए एक विश्लेषक।
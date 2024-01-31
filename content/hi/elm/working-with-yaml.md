---
title:                "यामल के साथ काम करना"
date:                  2024-01-19
html_title:           "C#: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

YAML, "YAML Ain't Markup Language" के लिए खड़ा है, जो डेटा सिरिएलाइजेशन फॉरमेट है। प्रोग्रामर इसे कॉन्फ़िगरेशन फाइल्स, डेटा एक्सचेंज और बहुत से अन्य कामों के लिए इस्तेमाल करते हैं क्योंकि यह समझने में आसान और मशीन के लिए पढ़ने योग्य होता है।

## How to: (कैसे करें:)

Elm में डायरेक्ट YAML पार्सिंग की कोई लाइब्रेरी नहीं है, लेकिन आप JSON लाइब्रेरी के साथ इंटरऑपरेट कर सकते हैं। नीचे हम देखेंगे कि YAML को JSON में कैसे बदलें और Elm में काम करें।

```Elm
-- नोट: Elm में प्रत्यक्ष YAML पार्सिंग सपोर्ट नहीं है।
-- हम अपेक्षा करते हैं कि सर्वर पर YAML को JSON में कनवर्ट किया जा चूका है।

import Json.Decode exposing (decodeString, string)

type alias Config =
    { name : String
    , version : String
    }

configDecoder : Json.Decode.Decoder Config
configDecoder =
    Json.Decode.map2 Config
        (Json.Decode.field "name" string)
        (Json.Decode.field "version" string)

configJson : String
configJson = "{\"name\":\"myProject\", \"version\":\"1.0.0\"}"

result : Result String Config
result =
    decodeString configDecoder configJson

-- इसका आउटपुट एक Config प्रकार का रिजल्ट होगा।
```

## Deep Dive (गहराई से जानकारी)

इतिहास में, YAML का उद्भव मानव-पठनीयता को महत्व देने वाले डेटा फॉरमेट के रूप में हुआ। JSON और XML जैसे विकल्प हैं, लेकिन YAML अक्सर इसकी सादगी के लिए चुना जाता है। Elm में YAML के साथ काम करने के लिए आमतौर पर सर्वर-साइड पर YAML डेटा को JSON में बदलना पड़ता है, क्योंकि Elm में यह सीधे समर्थित नहीं है।

## See Also (और भी देखें)

- YAML आधिकारिक वेबसाइट: [YAML](https://yaml.org/)
- Elm JSON पार्सिंग गाइड: [Elm Guide - JSON](https://guide.elm-lang.org/effects/json.html)
- YAML to JSON कन्वर्टर टूल: [YAML to JSON Converter](https://www.json2yaml.com/)
- Elm पैकेज: [Elm Packages](https://package.elm-lang.org/)

---
title:                "एक तारीख को स्ट्रिंग में परिवर्तित करना"
html_title:           "Java: एक तारीख को स्ट्रिंग में परिवर्तित करना"
simple_title:         "एक तारीख को स्ट्रिंग में परिवर्तित करना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# एल्म अनुभाग कहानी: तारीख को स्ट्रिंग में बदलना

## क्या और क्यों?
तारीख को स्ट्रिंग में बदलना एक ऐसी प्रक्रिया है जिसमें एक प्रोग्रामर डेट को टेक्स्ट यानी स्ट्रिंग में बदलता है। प्रोग्रामर इसे उपयोगकर्ता इंटरफ़ेस में दिखाने, लोग्स लिखने या डाटा को आसानी से प्रिंट करने के लिए करते हैं। 

## कैसे करें:
Elm मॐ टारीख को माइक्रोसेकंड्स में स्टोर किया जाता है, और इसे ज़्यादा पढ़ने योग्य बनाया जा सकता है। इसके लिए, हमें 'elm/time' पैकेज की सहायता चाहिए होगी।

```Elm
import Time exposing (..)
import Time.Extra exposing (..)

viewTime: Zone -> Posix -> Html msg
viewTime zone posix =
    text (toString (toOffsetTime zone posix))

-- उदाहारण टाइमजोन के लिए
let
    posix = Time.millisToPosix 1630705367008
    timeZone = Time.utc
in
viewTime timeZone posix
```

नीचे की तरह प्रिंट होता है:
```
"2021-09-04T08:56:07.008Z"
```

## गहराई की बातें
1. **ऐतिहासिक संदर्भ**: जब एल्म बनाया गया था, तारीखों को संभालने के बहुत कम तरीके थे। इसे स्थायी और सुरक्षित बनाने के लिए, एल्म ने इसे Milliseconds पोज़िक्स इन्टज़ के रूप में किया।
2. **विकल्प**: आप भी 'Date' पैकेज का उपयोग कर सकते हैं, यह उपयोगकर्ता को ज्यादा Flexibility देता है लेकिन इसे मैंन्टेन करना कठिन हो जाता है।
3. **प्रदर्शन विवरण**: टाइमज़ोन की जानकारी मौजूद होने पर, 'toOffsetTime' फ़ंक्शन POSIX समय को 'OffsetTime' में बदल देता है और इसे सही दिखाने के लिए 'toString' इस्तेमाल किया जाता है। 

## अधिक जानके लिए
* Elm टाइम डॉक्यूमेंटेशन: [http://package.elm-lang.org/packages/elm/time/latest](http://package.elm-lang.org/packages/elm/time/latest)
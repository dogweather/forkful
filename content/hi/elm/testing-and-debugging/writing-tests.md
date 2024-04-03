---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:20.685403-07:00
description: "\u0915\u0948\u0938\u0947: Elm \u092F\u0942\u0928\u093F\u091F \u0914\u0930\
  \ \u092B\u093C\u091C\u093C \u092A\u0930\u0940\u0915\u094D\u0937\u0923 \u0932\u093F\
  \u0916\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F `elm-explorations/test` \u092A\
  \u0948\u0915\u0947\u091C \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\
  \u0924\u093E \u0939\u0948\u0964 \u0905\u092A\u0928\u0947 \u092A\u094D\u0930\u094B\
  \u091C\u0947\u0915\u094D\u091F \u092E\u0947\u0902 \u092A\u0948\u0915\u0947\u091C\
  \ \u0915\u094B \u091C\u094B\u0921\u093C\u0928\u093E \u0936\u0941\u0930\u0942 \u0915\
  \u0930\u0947\u0902."
lastmod: '2024-03-13T22:44:52.192756-06:00'
model: gpt-4-0125-preview
summary: "Elm \u092F\u0942\u0928\u093F\u091F \u0914\u0930 \u092B\u093C\u091C\u093C\
  \ \u092A\u0930\u0940\u0915\u094D\u0937\u0923 \u0932\u093F\u0916\u0928\u0947 \u0915\
  \u0947 \u0932\u093F\u090F `elm-explorations/test` \u092A\u0948\u0915\u0947\u091C\
  \ \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0924\u093E \u0939\u0948\
  \u0964 \u0905\u092A\u0928\u0947 \u092A\u094D\u0930\u094B\u091C\u0947\u0915\u094D\
  \u091F \u092E\u0947\u0902 \u092A\u0948\u0915\u0947\u091C \u0915\u094B \u091C\u094B\
  \u0921\u093C\u0928\u093E \u0936\u0941\u0930\u0942 \u0915\u0930\u0947\u0902."
title: "\u091F\u0947\u0938\u094D\u091F \u0932\u093F\u0916\u0928\u093E"
weight: 36
---

## कैसे:
Elm यूनिट और फ़ज़ परीक्षण लिखने के लिए `elm-explorations/test` पैकेज का उपयोग करता है। अपने प्रोजेक्ट में पैकेज को जोड़ना शुरू करें:

```elm
elm install elm-explorations/test
```

एक परीक्षण फाइल बनाएं, `tests/ExampleTest.elm`, और परीक्षण मॉड्यूल्स को इंपोर्ट करें। यहां एक सरल परीक्षण है जो एक फंक्शन `add : Int -> Int -> Int` को सत्यापित करता है:

```elm
module ExampleTest exposing (..)

import Expect
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "A simple addition function"
        [ test "Adding 2 and 3 yields 5" <| 
            \_ -> add 2 3 |> Expect.equal 5
        ]

```

अपने परीक्षण चलाने के लिए, आपको `elm-test` की आवश्यकता होगी:

```shell
npm install -g elm-test
elm-test
```

यह आपके परीक्षणों को संकलित करेगा और आपके टर्मिनल में परिणाम प्रिंट करेगा। ऊपर दिए गए उदाहरण के लिए, आउटपुट कुछ इस तरह होगा:

```
TEST RUN PASSED

Duration: 42 ms
Passed:   1
Failed:   0
```

एक और जटिल उदाहरण के लिए, मान लें कि आप `add` फंक्शन को विभिन्न प्रकार के इंटेजर इनपुट्स को सही ढंग से संभालने के लिए फ़ज़ परीक्षण करना चाहते हैं। आप अपने `ExampleTest.elm` को इस प्रकार से संशोधित करेंगे:

```elm
module ExampleTest exposing (..)

import Expect
import Fuzz exposing (int)
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "Testing add with fuzzing"
        [ fuzz int "Fuzz testing add with random ints" <| 
            \int1 int2 -> add int1 int2 |> Expect.equal (int1 + int2)
        ]
```

`elm-test` फिर से चलाएं और फ़ज़ परीक्षणों को क्रियान्वित होते देखें। आउटपुट यादृच्छिक इनपुट के साथ भिन्न होगा लेकिन सफल परीक्षण विफलता की कोई सूचना नहीं देगा:

```
TEST RUN PASSED

Duration: 183 ms
Passed:   100
Failed:   0
``` 

ये उदाहरण दिखाते हैं कि कैसे Elm में सरल यूनिट और फ़ज़ परीक्षण लिखे और चलाए जाते हैं, `elm-explorations/test` पैकेज का उपयोग करके। परीक्षण विकास प्रक्रिया का एक महत्वपूर्ण हिस्सा है, यह सुनिश्चित करने में मदद करता है कि आपके Elm एप्लिकेशन विश्वसनीय हैं और उच्च गुणवत्ता बनाए रखते हैं।

---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:20.685403-07:00
description: "Elm \u092E\u0947\u0902 \u092A\u0930\u0940\u0915\u094D\u0937\u0923 \u0932\
  \u093F\u0916\u0928\u093E \u0906\u092A\u0915\u0947 Elm \u0915\u094B\u0921 \u0915\u0940\
  \ \u0936\u0941\u0926\u094D\u0927\u0924\u093E \u0915\u0940 \u091C\u093E\u0902\u091A\
  \ \u0915\u0947 \u0932\u093F\u090F \u092A\u0930\u0940\u0915\u094D\u0937\u0923 \u092E\
  \u093E\u092E\u0932\u0947 \u092C\u0928\u093E\u0928\u0947 \u0935\u093E\u0932\u0940\
  \ \u092A\u094D\u0930\u0915\u094D\u0930\u093F\u092F\u093E \u0939\u0948, \u092F\u0939\
  \ \u0938\u0941\u0928\u093F\u0936\u094D\u091A\u093F\u0924 \u0915\u0930\u0928\u093E\
  \ \u0915\u093F \u092F\u0939 \u0909\u092E\u094D\u092E\u0940\u0926 \u0915\u0947 \u092E\
  \u0941\u0924\u093E\u092C\u093F\u0915 \u0915\u093E\u0930\u094D\u092F \u0915\u0930\
  \u0924\u093E\u2026"
lastmod: '2024-03-13T22:44:52.192756-06:00'
model: gpt-4-0125-preview
summary: "Elm \u092E\u0947\u0902 \u092A\u0930\u0940\u0915\u094D\u0937\u0923 \u0932\
  \u093F\u0916\u0928\u093E \u0906\u092A\u0915\u0947 Elm \u0915\u094B\u0921 \u0915\u0940\
  \ \u0936\u0941\u0926\u094D\u0927\u0924\u093E \u0915\u0940 \u091C\u093E\u0902\u091A\
  \ \u0915\u0947 \u0932\u093F\u090F \u092A\u0930\u0940\u0915\u094D\u0937\u0923 \u092E\
  \u093E\u092E\u0932\u0947 \u092C\u0928\u093E\u0928\u0947 \u0935\u093E\u0932\u0940\
  \ \u092A\u094D\u0930\u0915\u094D\u0930\u093F\u092F\u093E \u0939\u0948, \u092F\u0939\
  \ \u0938\u0941\u0928\u093F\u0936\u094D\u091A\u093F\u0924 \u0915\u0930\u0928\u093E\
  \ \u0915\u093F \u092F\u0939 \u0909\u092E\u094D\u092E\u0940\u0926 \u0915\u0947 \u092E\
  \u0941\u0924\u093E\u092C\u093F\u0915 \u0915\u093E\u0930\u094D\u092F \u0915\u0930\
  \u0924\u093E\u2026"
title: "\u091F\u0947\u0938\u094D\u091F \u0932\u093F\u0916\u0928\u093E"
weight: 36
---

## क्या और क्यों?

Elm में परीक्षण लिखना आपके Elm कोड की शुद्धता की जांच के लिए परीक्षण मामले बनाने वाली प्रक्रिया है, यह सुनिश्चित करना कि यह उम्मीद के मुताबिक कार्य करता है। प्रोग्रामर्स इसे जल्दी से बग्स पकड़ने, रख-रखाव को आसान बनाने, और उनके एप्लिकेशन की गुणवत्ता और विश्वसनीयता में सुधार के लिए करते हैं।

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

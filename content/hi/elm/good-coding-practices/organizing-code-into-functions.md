---
date: 2024-01-26 01:10:54.830389-07:00
description: "\u0905\u092A\u0928\u0947 \u0938\u093E\u0930\u0947 \u0915\u094B\u0921\
  \ \u0915\u094B \u090F\u0915 \u092C\u0921\u093C\u0947 \u0922\u0947\u0930 \u092E\u0947\
  \u0902 \u0921\u093E\u0932 \u0926\u0947\u0928\u093E? \u092C\u0941\u0930\u093E \u0935\
  \u093F\u091A\u093E\u0930 \u0939\u0948\u0964 \u0907\u0938\u0947 \u092B\u093C\u0902\
  \u0915\u094D\u0936\u0928\u094D\u0938 \u092E\u0947\u0902 \u092C\u093E\u0901\u091F\
  \u0928\u093E? \u0905\u091A\u094D\u091B\u093E \u0935\u093F\u091A\u093E\u0930 \u0939\
  \u0948\u0964 \u092F\u0939 \u0906\u092A\u0915\u0947 Elm \u0915\u094B\u0921 \u0915\
  \u094B \u0938\u093E\u092B\u093C, \u092A\u0941\u0928\u0903 \u092A\u094D\u0930\u092F\
  \u094B\u0917 \u0915\u0930\u0928\u0947 \u092F\u094B\u0917\u094D\u092F \u0914\u0930\
  \u2026"
lastmod: '2024-03-13T22:44:52.196202-06:00'
model: gpt-4-1106-preview
summary: "\u0905\u092A\u0928\u0947 \u0938\u093E\u0930\u0947 \u0915\u094B\u0921 \u0915\
  \u094B \u090F\u0915 \u092C\u0921\u093C\u0947 \u0922\u0947\u0930 \u092E\u0947\u0902\
  \ \u0921\u093E\u0932 \u0926\u0947\u0928\u093E? \u092C\u0941\u0930\u093E \u0935\u093F\
  \u091A\u093E\u0930 \u0939\u0948\u0964 \u0907\u0938\u0947 \u092B\u093C\u0902\u0915\
  \u094D\u0936\u0928\u094D\u0938 \u092E\u0947\u0902 \u092C\u093E\u0901\u091F\u0928\
  \u093E? \u0905\u091A\u094D\u091B\u093E \u0935\u093F\u091A\u093E\u0930 \u0939\u0948\
  \u0964 \u092F\u0939 \u0906\u092A\u0915\u0947 Elm \u0915\u094B\u0921 \u0915\u094B\
  \ \u0938\u093E\u092B\u093C, \u092A\u0941\u0928\u0903 \u092A\u094D\u0930\u092F\u094B\
  \u0917 \u0915\u0930\u0928\u0947 \u092F\u094B\u0917\u094D\u092F \u0914\u0930\u2026"
title: "\u0915\u094B\u0921 \u0915\u094B \u092B\u0902\u0915\u094D\u0936\u0928\u094D\
  \u0938 \u092E\u0947\u0902 \u0935\u094D\u092F\u0935\u0938\u094D\u0925\u093F\u0924\
  \ \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?
अपने सारे कोड को एक बड़े ढेर में डाल देना? बुरा विचार है। इसे फ़ंक्शन्स में बाँटना? अच्छा विचार है। यह आपके Elm कोड को साफ़, पुनः प्रयोग करने योग्य और परीक्षण करने में आसान बनाता है। अपने कोड को फ़ंक्शन्स में व्यवस्थित करके, आप विशेष कार्य करने वाले कोड को साथ में समूहित करते हैं, जो आपके आवेदन को अधिक रखरखाव और समझने में आसान बनाता है।

## कैसे करें:
यहाँ एक Elm कोड का टुकड़ा है जिसमें एक साधारण फ़ंक्शन है जो उपयोगकर्ता को अभिवादन करता है:

```Elm
module Main exposing (..)

import Html exposing (text)

greetUser : String -> String
greetUser userName =
    "Hello, " ++ userName ++ "!"

main =
    text (greetUser "Casey")
```

इसे चलाएँ, और आपको आउटपुट मिलेगा: "Hello, Casey!"

अब, मान लीजिए आप और वैयक्तीकरण जोड़ना चाहते हैं। और फंक्शनलिटी को निकालें!

```Elm
module Main exposing (..)

import Html exposing (text)

greetUser : String -> String -> String
greetUser greeting userName =
    greeting ++ ", " ++ userName ++ "!"

personalGreeting : String -> String
personalGreeting userName =
    greetUser "Howdy" userName

main =
    text (personalGreeting "Casey")
```

अब, जब आप इसे चलाएंगे: "Howdy, Casey!" जादू? नहीं, बस फ़ंक्शन्स अपना काम कर रहे हैं।

## गहराई से जानकारी
पुराने दिनों में, कोड अक्सर एक लंबी श्रंखला के निर्देशों का होता था (स्पेगेटी कोड के बारे में सोचें)। इसे रखरखाव की दृष्टि से एक बुरा सपना था। फिर संरचित प्रोग्रामिंग आई, और उसके साथ, फ़ंक्शन्स। Elm, अपने फंक्शनल प्रोग्रामिंग पूर्वजों की तरह, संगठन के लिए फ़ंक्शन्स पर भारी निर्भरता रखता है।

आप फ़ंक्शन्स को नेस्ट कर सकते हैं, क्लोजर्स बना सकते हैं, या सरलता के लिए उन्हें शुद्ध रख सकते हैं। Elm दूसरे को प्रोत्साहित करता है: शुद्ध फ़ंक्शन्स जिनके इनपुट और आउटपुट अच्छी तरह से परिभाषित होते हैं, जिससे डीबगिंग और परीक्षण करने में आसानी होती है।

Elm फ़ंक्शन्स उच्च-क्रम के भी हो सकते हैं, अर्थात वे अन्य फ़ंक्शन्स को स्वीकार कर सकते हैं या वापस कर सकते हैं। यह संरचनात्मकता की एक दुनिया को खोलता है। हालाँकि, कुछ अन्य भाषाओं के विपरीत, Elm में फंक्शन ओवरलोडिंग नहीं होती; प्रत्येक फ़ंक्शन का एक अनूठा नाम होना चाहिए।

इसके अलावा, Elm एक मजबूत स्टैटिक टाइपिंग प्रणाली लगाता है जो सिर्फ टाइप्स की जांच नहीं करता बल्कि उन्हें समझता भी है, जिससे बेकार का कोड कम होता है।

अन्य भाषाओं में प्रक्रियात्मक या वस्तु-उन्मुख कोड संगठन की तुलना में, Elm का दृष्टिकोण सरलता और पूर्वानुमान लगाने योग्यता पर जोर देता है। Elm में वस्तुएँ या कक्षाएँ नहीं होतीं। आप कक्षाएँ और उदाहरणों के बजाय फ़ंक्शन्स और मॉड्यूल के साथ कोड को व्यवस्थित करते हैं।

## यह भी देखें
अधिक जानकारी के लिए, इन संसाधनों को देखें:
- फ़ंक्शन्स पर Elm की आधिकारिक गाइड: https://guide.elm-lang.org/core_language.html
- अधिक जटिल फ़ंक्शन उदाहरणों के लिए Elm पैकेज डॉक्यूमेंटेशन: https://package.elm-lang.org/
- Elm की टाइप प्रणाली के बारे में जानें, जो फ़ंक्शन संगठन के साथ अच्छे से मेल खाती है: https://elm-lang.org/docs/types

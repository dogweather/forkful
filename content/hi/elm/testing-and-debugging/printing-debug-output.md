---
date: 2024-01-20 17:52:30.461482-07:00
description: "\u0921\u0940\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F \u092A\
  \u094D\u0930\u093F\u0902\u091F \u0915\u0930\u0928\u0947 \u0915\u093E \u092E\u0924\
  \u0932\u092C \u0939\u0948 \u0915\u094B\u0921 \u092E\u0947\u0902 \u091C\u093E\u0928\
  \u0915\u093E\u0930\u0940 \u0932\u0949\u0917 \u0915\u0930\u0928\u093E, \u091C\u093F\
  \u0938\u0938\u0947 \u092C\u0917\u094D\u0938 \u0914\u0930 \u0938\u092E\u0938\u094D\
  \u092F\u093E\u090F\u0902 \u0922\u0942\u0901\u0922\u0928\u093E \u0906\u0938\u093E\
  \u0928 \u0939\u094B \u091C\u093E\u0924\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\
  \u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947 \u0905\
  \u092A\u0928\u0947 \u0915\u094B\u0921 \u0915\u0940 \u091C\u093E\u0902\u091A-\u092A\
  \u0921\u093C\u0924\u093E\u0932 \u0915\u0947\u2026"
lastmod: '2024-03-13T22:44:52.191051-06:00'
model: gpt-4-1106-preview
summary: "\u0921\u0940\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F \u092A\u094D\
  \u0930\u093F\u0902\u091F \u0915\u0930\u0928\u0947 \u0915\u093E \u092E\u0924\u0932\
  \u092C \u0939\u0948 \u0915\u094B\u0921 \u092E\u0947\u0902 \u091C\u093E\u0928\u0915\
  \u093E\u0930\u0940 \u0932\u0949\u0917 \u0915\u0930\u0928\u093E, \u091C\u093F\u0938\
  \u0938\u0947 \u092C\u0917\u094D\u0938 \u0914\u0930 \u0938\u092E\u0938\u094D\u092F\
  \u093E\u090F\u0902 \u0922\u0942\u0901\u0922\u0928\u093E \u0906\u0938\u093E\u0928\
  \ \u0939\u094B \u091C\u093E\u0924\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\
  \u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947 \u0905\u092A\
  \u0928\u0947 \u0915\u094B\u0921 \u0915\u0940 \u091C\u093E\u0902\u091A-\u092A\u0921\
  \u093C\u0924\u093E\u0932 \u0915\u0947\u2026"
title: "\u0921\u0940\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F \u092A\u094D\
  \u0930\u093F\u0902\u091F \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

डीबग आउटपुट प्रिंट करने का मतलब है कोड में जानकारी लॉग करना, जिससे बग्स और समस्याएं ढूँढना आसान हो जाता है। प्रोग्रामर्स इसे अपने कोड की जांच-पड़ताल के दौरान करते हैं ताकि प्रोग्राम सही ढंग से चल रहा है या नहीं ये जान सकें।

## How to: (कैसे करें:)

```Elm
import Browser
import Html exposing (text)
import Debug

main =
    let
        valueToCheck = "Hello, Elm!"
        -- डीबग लाइन जोड़ें
        _ = Debug.log "DebugOutput" valueToCheck
    in
    -- सिंपल HTML पेज
    Browser.sandbox { init = valueToCheck, update = \_ model -> model, view = text }

```
सैंपल आउटपुट:
```
DebugOutput: "Hello, Elm!"
```

## Deep Dive (गहराई में जानकारी):

डीबगिंग की जड़ें प्रोग्रामिंग के शुरुआती दिनों से हैं। Elm में `Debug.log` फ़ंक्शन पुराने प्रिंट स्टेटमेंट्स का एक सुधारित रूप है। इसका इस्तेमाल करके आप वैल्यूज़ और फ़ंक्शन कॉल्स को कंसोल में लॉग कर सकते हैं जो डीबगिंग में बहुत मददगार होता है। वैकल्पिक रूप से, Elm 0.19 में `Debug.todo` और `Debug.toString` जैसे कुछ और टूल्स भी हैं, लेकिन याद रखें कि `Debug` मॉड्यूल को प्रोडक्शन कोड में इस्तेमाल करने की अनुमति नहीं है। बेस्ट प्रैक्टिस ये है कि डीबगिंग के बाद सारे `Debug` स्टेटमेंट्स को निकाल दिया जाए।

## See Also (और भी जानकारी):

- ऑनलाइन Elm कम्पाइलर: [Elm Online Compiler](https://elm-lang.org/try)
- Elm पैकेज: Debug: [Elm Package - Debug](https://package.elm-lang.org/packages/elm/core/latest/Debug)

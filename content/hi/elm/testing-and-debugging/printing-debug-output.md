---
date: 2024-01-20 17:52:30.461482-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0938\u0948\
  \u0902\u092A\u0932 \u0906\u0909\u091F\u092A\u0941\u091F."
lastmod: '2024-04-05T21:53:54.197079-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0938\u0948\u0902\u092A\
  \u0932 \u0906\u0909\u091F\u092A\u0941\u091F."
title: "\u0921\u0940\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F \u092A\u094D\
  \u0930\u093F\u0902\u091F \u0915\u0930\u0928\u093E"
weight: 33
---

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

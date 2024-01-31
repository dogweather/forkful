---
title:                "डीबग आउटपुट प्रिंट करना"
date:                  2024-01-20T17:52:30.461482-07:00
model:                 gpt-4-1106-preview
simple_title:         "डीबग आउटपुट प्रिंट करना"

category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/printing-debug-output.md"
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

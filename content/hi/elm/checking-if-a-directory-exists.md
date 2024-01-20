---
title:                "डायरेक्टरी का अस्तित्व जाँचना"
date:                  2024-01-20T14:56:22.664483-07:00
html_title:           "Elm: डायरेक्टरी का अस्तित्व जाँचना"
simple_title:         "डायरेक्टरी का अस्तित्व जाँचना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
डायरेक्टरी की मौजूदगी जांचने का मतलब है कि किसी फाइल सिस्टम में एक विशेष फोल्डर मौजूद है या नहीं। प्रोग्रामर्स डाटा को पढ़ने या लिखने से पहले यह जांचते हैं ताकि एरर को रोका जा सके।

## How to: (कैसे करें?)
Elm भाषा में सीधे तरीके से फ़ाइल सिस्टम का एक्सेस नहीं होता, इसलिए आपको बाहरी जावास्क्रिप्ट इंटरफेस (JavaScript Interop) का इस्तेमाल करना पड़ेगा जैसे कि Ports या WebAssembly मॉड्यूल। यहां एक साधारण उदाहरण है:

```
port module Main exposing (..)

-- Define a port to check the existence of a directory
port checkDirectory : String -> Cmd msg

-- Assuming you've set up a JavaScript function to listen for this port
-- and check the directory existence

...

-- To use the port, you might send a directory path like this:
Main.checkDirectory("/path/to/directory")
```

## Deep Dive (गहराई से जानकारी)
Elm में डायरेक्टरी की मौजूदगी जांचना सीधे नहीं किया जा सकता क्योंकि Elm को ब्राउज़र के लिए बनाया गया है और फ़ाइल सिस्टम का एक्सेस उसके सुरक्षा मॉडल में नहीं है। इसका अल्टरनेटिव है जावास्क्रिप्ट के साथ इंटरेक्शन, जिससे कि हम Elm से JavaScript के फ़ंक्शन को कॉल कर सकें। यह फ़ंक्शन Node.js का fs मॉड्यूल या ब्राउज़र APIs का इस्तेमाल करके चेक कर सकता है। 

बहुत समय पहले, जब ब्राउज़र और Node.js इतने विकसित नहीं थे, इस तरह के काम बहुत मुश्किल थे। लेकिन आज, HTML5 और प्रोग्रेसिव वेब एप्लीकेशन के उदय के चलते, वेब एप्लीकेशन फ़ाइल सिस्टम एक्सेस जैसी उन्नत क्षमताओं को प्रदान कर रहे हैं।

## See Also (और देखें)
1. Elm Ports: [Elm Ports](https://guide.elm-lang.org/interop/ports.html)
2. Using JavaScript with Elm: [Elm with JS](https://elm-lang.org/docs/from-javascript)
4. WebAssembly Introduction: [WebAssembly](https://developer.mozilla.org/en-US/docs/WebAssembly/Concepts)
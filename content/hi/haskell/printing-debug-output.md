---
title:                "डीबग आउटपुट प्रिंट करना"
html_title:           "Gleam: डीबग आउटपुट प्रिंट करना"
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

प्रिंटिंग डीबग आउटपुट प्रोग्राम की क्रियाएं अनुशासित करने के लिए एक तरीका है ताकि हम उनके वर्तमान स्थान और स्थिति का खुलासा कर सकें। प्रोग्रामर्स इसे मुख्यतः बग्स की खोज करने और उन्हें ठीक करने के लिए करते हैं। 

## कैसे करें:

```Haskell
import Debug.Trace

myFunction :: Int -> Int
myFunction x = trace ("x is: " ++ show x) (x + 1)

main = print $ myFunction 5
```

जब आप इसे चलाते हैं, तो आपको निम्न आउटपुट मिलेगा:

```Haskell
x is: 5
6
```
यहां, `trace` को "x is: " और `x` की मूल्य के साथ एक स्ट्रिंग दी जाती है, और फिर `x + 1` का परिणाम लौटाती है।

## गहरा डाइव:

**ऐतिहासिक संदर्भ:** Haskell में Debug.Trace लाइब्ररी ने एक नया युग शुरू किया था। पहले, डीबगिंग मूलतः एक नोटपैड और पेन का काम था, लेकिन एक स्पष्ट डीबग आउटपुट के साथ, दिशानिर्देश स्पष्ट हो गए। 

**विकल्प:** `putStrLn` फ़ंक्शन को भी उपयोग में लिया जा सकता है, लेकिन यह मूल्यों को बदल देता है और इसलिए प्रेडिक्टेबल नहीं होता है।

**आयामन विवरण:** `trace` पेयर्योक्सी मोनाड में चलता है, जिसे संभवतः मौन प्रवाह इनपुट के रूप में चलाया जा सकता है। यह सुनिश्चित करता है कि आउटपुट क्रम में ही होगा।

## यहां भी देखें:

[Haskell स्रोत पाठ्यक्रम](https://www.haskell.org/tutorial/) और ['Debug.Trace' एपीआई डॉक्यूमेंटेशन](https://hackage.haskell.org/package/base-4.14.1.0/docs/Debug-Trace.html) क्वरी करने के लिए बहुत उपयोगी रिसोर्स हैं। आप [Real World Haskell](https://book.realworldhaskell.org/) को भी देख सकते हैं, जो की एक खुला हास्केल पाठ्यक्रम है।
---
title:                "डीबग आउटपुट प्रिंट करना"
date:                  2024-01-20T17:52:54.889247-07:00
model:                 gpt-4-1106-preview
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
डीबग आउटपुट प्रिंटिंग का मतलब है प्रोग्राम के चलाने के दौरान सूचनाओं को दिखाना, ताकि कोड कैसे काम कर रहा है, यह समझा जा सके। प्रोग्रामर्स इसे बग्स ढूंढने और फिक्स करने के लिए करते हैं।

## कैसे करें? (How to:)
```Haskell
main :: IO ()
main = do
  putStrLn "डीबग: मुख्य फ़ंक्शन शुरू हो गया है।"
  let result = calculate 5
  putStrLn $ "डीबग: परिणाम है: " ++ show result

calculate :: Int -> Int
calculate x = x + 1
```
सैंपल आउटपुट:
```
डीबग: मुख्य फ़ंक्शन शुरू हो गया है।
डीबग: परिणाम है: 6
```

## गहराई से समझें (Deep Dive)
डीबगिंग आउटपुट का इस्तेमाल हास्केल में 'print' या 'putStrLn' फ़ंक्शन के जरिए होता है, जो 'Prelude' मॉड्यूल में शामिल है। इतिहास में देखें, तो शुरुआती प्रोग्राम्मिंग भाषाओं से ही डीबगिंग का चलन था। हास्केल में 'Debug.Trace' मॉड्यूल भी है, लेकिन यह केवल अस्थायी डीबगिंग के लिए अच्छा है, क्योंकि इसका उपयोग प्रोग्राम की शुद्धता को बाधित कर सकता है। वैकल्पिक तरीकों में 'printf' डीबगिंग और विभिन्न लॉगिंग लाइब्रेरीज़ शामिल हैं।

## और देखें (See Also)
- हास्केल डॉक्स - 'Prelude' मॉड्यूल: [Haskell Prelude](https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html)
- हास्केल डॉक्स - 'Debug.Trace' मॉड्यूल: [Debug.Trace](https://hackage.haskell.org/package/base-4.14.0.0/docs/Debug-Trace.html)

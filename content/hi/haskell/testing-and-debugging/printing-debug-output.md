---
date: 2024-01-20 17:52:54.889247-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902? (How to:) \u0938\u0948\
  \u0902\u092A\u0932 \u0906\u0909\u091F\u092A\u0941\u091F."
lastmod: '2024-04-05T22:38:53.317257-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u0921\u0940\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F \u092A\u094D\
  \u0930\u093F\u0902\u091F \u0915\u0930\u0928\u093E"
weight: 33
---

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

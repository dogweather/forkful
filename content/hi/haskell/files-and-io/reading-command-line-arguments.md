---
date: 2024-01-20 17:56:10.679407-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Haskell\
  \ \u092E\u0947\u0902 \u0915\u092E\u093E\u0902\u0921 \u0932\u093E\u0907\u0928 \u0906\
  \u0930\u094D\u0917\u094D\u092F\u0942\u092E\u0947\u0902\u091F\u094D\u0938 \u092A\u0922\
  \u093C\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F."
lastmod: '2024-04-05T21:53:54.420635-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Haskell \u092E\u0947\
  \u0902 \u0915\u092E\u093E\u0902\u0921 \u0932\u093E\u0907\u0928 \u0906\u0930\u094D\
  \u0917\u094D\u092F\u0942\u092E\u0947\u0902\u091F\u094D\u0938 \u092A\u0922\u093C\u0928\
  \u0947 \u0915\u0947 \u0932\u093F\u090F."
title: "\u0915\u092E\u093E\u0902\u0921 \u0932\u093E\u0907\u0928 \u0906\u0930\u094D\
  \u0917\u0941\u092E\u0947\u0902\u091F\u094D\u0938 \u092A\u0922\u093C\u0928\u093E"
weight: 23
---

## How to: (कैसे करें:)
Haskell में कमांड लाइन आर्ग्यूमेंट्स पढ़ने के लिए:

```Haskell
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    print args
```

इस कोड के चलने पर, जो आर्ग्यूमेंट्स आपने कमांड लाइन पर दिए होंगे, वो एक लिस्ट में प्रिंट होंगे।

सैंपल आउटपुट:
```
$ runhaskell MyProgram.hs arg1 arg2 arg3
["arg1", "arg2", "arg3"]
```

## Deep Dive: (गहराई में जानकारी:)
कमांड लाइन आर्ग्यूमेंट्स का इस्तेमाल 1960s से हो रहा है; Unix पर बेस्ड सिस्टम्स ने इसे काफी लोकप्रिय बनाया। Haskell में `getArgs` फंक्शन `System.Environment` मॉड्यूल का हिस्सा है और इसे सीधे लिस्ट के फॉर्म में आर्ग्यूमेंट्स प्राप्त करने के लिए उपयोग किया जाता है।

एल्टरनेटिव के रूप में, `optparse-applicative` जैसे लाइब्रेरी का उपयोग करना संभव है जो कि मोर सोफिस्टिकेटेड कमांड लाइन पार्सिंग ऑप्शन्स प्रदान करते हैं।

## See Also: (अधिक जानकारी के लिए:)
- Haskell `System.Environment` मॉड्यूल: [Haskell Documentation](https://hackage.haskell.org/package/base-4.16.0.0/docs/System-Environment.html)
- `optparse-applicative` लाइब्रेरी: [optparse-applicative on Hackage](https://hackage.haskell.org/package/optparse-applicative)

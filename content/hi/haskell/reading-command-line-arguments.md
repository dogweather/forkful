---
title:                "कमांड लाइन आर्गुमेंट्स पढ़ना"
date:                  2024-01-20T17:56:10.679407-07:00
model:                 gpt-4-1106-preview
simple_title:         "कमांड लाइन आर्गुमेंट्स पढ़ना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (क्या है और क्यों?)

कमांड लाइन आर्ग्यूमेंट्स हमें प्रोग्राम चलाते वक्त इनपुट देने का एक तरीका है। इनका उपयोग इसलिए होता है क्योंकि ये फ्लेक्सिबलिटी देते हैं और यूजर को प्रोग्राम के व्यवहार को कण्ट्रोल करने की सुविधा प्रदान करते हैं।

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
---
date: 2024-01-20 17:54:29.021279-07:00
description: "How to: (\u0915\u0948\u0938\u0947:) Haskell \u092E\u0947\u0902 \u092A\
  \u093E\u0920 \u092B\u093C\u093E\u0907\u0932 \u092A\u0922\u093C\u0928\u093E \u0938\
  \u0940\u0927\u093E \u0939\u0948."
lastmod: '2024-03-13T22:44:52.434891-06:00'
model: gpt-4-1106-preview
summary: "Haskell \u092E\u0947\u0902 \u092A\u093E\u0920 \u092B\u093C\u093E\u0907\u0932\
  \ \u092A\u0922\u093C\u0928\u093E \u0938\u0940\u0927\u093E \u0939\u0948."
title: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\u0907\u0932\
  \ \u092A\u0922\u093C\u0928\u093E"
weight: 22
---

## How to: (कैसे:)
Haskell में पाठ फ़ाइल पढ़ना सीधा है:

```Haskell
import System.IO

main :: IO ()
main = do
    contents <- readFile "hello.txt"
    putStrLn contents
```

जब `hello.txt` में `"Hello, Haskell!"` होगा, आउटपुट होगा:

```
Hello, Haskell!
```

## Deep Dive (गहराई से जानकारी)
पाठ फ़ाइलों को पढ़ना आरंभिक कंप्यूटिंग युग से है। Haskell में, `readFile` एक लेज़ी फंक्शन है जो फ़ाइल को धीरे-धीरे पढ़ता है जब जरुरत होती है। वैकल्पिक तरीके में `Data.ByteString` लाइब्रेरी फ़ाइल को बाइनरी फॉर्म में पढ़ती है, जो बड़े डेटा के लिए उपयुक्त हो सकती है।

सावधानी: `readFile` को `System.IO` मॉड्यूल से इम्पोर्ट करना जरुरी है, और फ़ाइल की पाठ्य सामग्री को संभालने में ठीक एरर हैंडलिंग की आवश्यकता होती है।

## See Also (अधिक जानकारी के लिए)
- Haskell डॉक्स: [System.IO Module](https://hackage.haskell.org/package/base-4.16.1.0/docs/System-IO.html)
- Learn You a Haskell गाइड: [Input and Output chapter](http://learnyouahaskell.com/input-and-output)
- Real World Haskell पुस्तक: [Working with Files and Streams](http://book.realworldhaskell.org/read/io.html)

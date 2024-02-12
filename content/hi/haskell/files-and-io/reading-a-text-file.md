---
title:                "टेक्स्ट फ़ाइल पढ़ना"
aliases:
- /hi/haskell/reading-a-text-file.md
date:                  2024-01-20T17:54:29.021279-07:00
model:                 gpt-4-1106-preview
simple_title:         "टेक्स्ट फ़ाइल पढ़ना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

पाठ फ़ाइल को पढ़ना यानि फ़ाइल से डेटा एकत्र करना होता है। प्रोग्रामर्स यह अकसर एप्लिकेशन को जरुरी सूचनाएं देने या लॉग्स को प्रोसेस करने के लिए करते हैं।

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

---
title:                "मानक त्रुटि में लिखना"
date:                  2024-01-19
html_title:           "Arduino: मानक त्रुटि में लिखना"
simple_title:         "मानक त्रुटि में लिखना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

जब कोई प्रोग्राम रन करते हैं, तो आउटपुट (स्टैंडर्ड आउटपुट) और एरर मैसेज (स्टैंडर्ड एरर) दो अलग-अलग जगहों पर भेजे जाते हैं। एरर मैसेज को स्टैंडर्ड एरर में लिखने से प्रोग्राम की समस्याएं आसानी से ढूंढी और संभाली जा सकती हैं।

## How to: (कैसे करें?)

Haskell में स्टैंडर्ड एरर में लिखने के लिए 'System.IO' मॉड्यूल का उपयोग होता है। नीचे कोड और उदाहरण हैं:

```Haskell
import System.IO

main :: IO ()
main = do
    hPutStrLn stderr "यह एक एरर मैसेज है।"
```

इसे चलाने पर आपको अपने कंसोल पर एरर मैसेज दिखाई देगा।

## Deep Dive (गहराई से जानकारी)

Haskell में 'stderr' 'Handle' का प्रयोग ऐतिहासिक Unix कंसेप्ट्स पर आधारित है जहाँ स्टैंडर्ड स्ट्रीम्स (stdin, stdout, और stderr) का उपयोग होता है। वैकल्पिक रूप में, 'System.Log.Logger' जैसे लॉगिंग लाइब्रेरीज़ भी होती हैं, परंतु 'stderr' सीधा और तेज़ तरीका है। इंटर्नली, Haskell 'stderr' को फाइल डिस्क्रिप्टर 2 से मैप करता है।

## See Also (और भी देखें)

- Haskell का ऑफिशियल डॉक्यूमेंटेशन: [System.IO](https://hackage.haskell.org/package/base-4.14.0.0/docs/System-IO.html)
- Unix स्टैंडर्ड स्ट्रीम्स: [Wikipedia](https://en.wikipedia.org/wiki/Standard_streams)
- Haskell लॉगिंग फैसिलिटीज़: [hackage.haskell.org](https://hackage.haskell.org/packages/#cat:Logging)

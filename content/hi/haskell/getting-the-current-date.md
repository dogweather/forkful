---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "C#: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
वर्तमान तारीख प्राप्त करना इसका मतलब है की आप उस समय की तारीख प्राप्त करने की कोशिश कर रहे हैं जब यह कोड चलाया जाता है । प्रोग्रामर इसे हाँडल करने, समय-संबंधी लॉजिक को कार्यान्वित करने और डेटा को ट्रैक करने के लिए करते हैं।

## कैसे करें:
Haskell में, हम `Data.Time` पैकेज का उपयोग करके वर्तमान तारीख प्राप्त कर सकते हैं।

```Haskell
import Data.Time

main :: IO ()
main = do
    now <- getCurrentTime
    putStrLn $ "अभी का समय है: " ++ show now
```
और आउटपुट होगा:

```Haskell
अभी का समय है: 2022-03-09 17:20:03.6288822 UTC
```

## गहरा डाइव
Haskell में `Data.Time` पैकेज 2006 में पेश किया गया था। यह व्यापक और विस्तारित समय एपीआई प्रदान करती है। `getCurrentTime` के विकल्प स्वरूप आप `Data.Time.Clock.POSIX` का उपयोग कर सकते हैं जो POSIX इपॉक को प्राप्त करेगा।

```Haskell
import Data.Time.Clock.POSIX

currentTime :: IO POSIXTime
currentTime = getPOSIXTime
```

## भी देखें
अधिक जानकारी के लिए जरूर देखें:
1. Haskell का आधिकारिक डॉक्युमेंटेशन `Data.Time`: [यहां](https://hackage.haskell.org/package/time-1.11.1.1/docs/Data-Time.html)
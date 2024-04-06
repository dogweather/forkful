---
date: 2024-01-20 17:37:37.972299-07:00
description: "\u0924\u0930\u0940\u0915\u093E: \u0924\u093E\u0930\u0940\u0916 \u0915\
  \u094B \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u092E\u0947\u0902 \u092C\
  \u0926\u0932\u0928\u0947 \u0915\u0940 \u092A\u094D\u0930\u0915\u094D\u0930\u093F\
  \u092F\u093E \u0915\u094B `Data.Time` \u0932\u093E\u0907\u092C\u094D\u0930\u0947\
  \u0930\u0940 \u092E\u0947\u0902 `formatTime` \u092B\u0902\u0915\u094D\u0936\u0928\
  \ \u0926\u094D\u0935\u093E\u0930\u093E \u0938\u092E\u094D\u092D\u0935 \u0915\u093F\
  \u092F\u093E \u0917\u092F\u093E \u0939\u0948, \u091C\u094B Haskell \u0915\u0947\
  \ \u092A\u0942\u0930\u094D\u0935 \u0935\u0930\u094D\u091C\u093C\u0928\u094D\u0938\
  \ \u0938\u0947\u2026"
lastmod: '2024-04-05T22:51:07.110584-06:00'
model: gpt-4-1106-preview
summary: "\u0924\u093E\u0930\u0940\u0916 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\
  \u093F\u0902\u0917 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u0947 \u0915\u0940\
  \ \u092A\u094D\u0930\u0915\u094D\u0930\u093F\u092F\u093E \u0915\u094B `Data.Time`\
  \ \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u092E\u0947\u0902 `formatTime`\
  \ \u092B\u0902\u0915\u094D\u0936\u0928 \u0926\u094D\u0935\u093E\u0930\u093E \u0938\
  \u092E\u094D\u092D\u0935 \u0915\u093F\u092F\u093E \u0917\u092F\u093E \u0939\u0948\
  , \u091C\u094B Haskell \u0915\u0947 \u092A\u0942\u0930\u094D\u0935 \u0935\u0930\u094D\
  \u091C\u093C\u0928\u094D\u0938 \u0938\u0947 \u0936\u093E\u092E\u093F\u0932 \u0939\
  \u0948\u0964 \u092F\u0939 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\
  \u0930 \u0915\u094B \u0935\u093F\u092D\u093F\u0928\u094D\u0928 \u0924\u093E\u0930\
  \u0940\u0916 \u0938\u094D\u0935\u0930\u0942\u092A\u094B\u0902 (formats) \u0915\u094B\
  \ \u091A\u0941\u0928\u0928\u0947 \u0915\u093E \u0935\u093F\u0915\u0932\u094D\u092A\
  \ \u0926\u0947\u0924\u093E \u0939\u0948, \u091C\u0948\u0938\u0947 \u0915\u093F \u0926\
  \u093F\u0928-\u092E\u0939\u0940\u0928\u093E-\u0935\u0930\u094D\u0937 (`\"%d-%m-%Y\"\
  `) \u092F\u093E \u092E\u0939\u0940\u0928\u093E-\u0926\u093F\u0928-\u0935\u0930\u094D\
  \u0937 (`\"%m/%d/%Y\"`)\u0964 \u0935\u093F\u0915\u0932\u094D\u092A \u0915\u0947\
  \ \u0924\u094C\u0930 \u092A\u0930, \u0905\u0928\u094D\u092F \u0939\u093E\u0908-\u0932\
  \u0947\u0935\u0932 \u0921\u0947\u091F\u094D\u0938 \u0939\u0948\u0902\u0921\u0932\
  \u093F\u0902\u0917 \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u093F\u091C\u093C\
  \ \u092D\u0940 \u0939\u094B\u0924\u0940 \u0939\u0948\u0902, \u091C\u0948\u0938\u0947\
  \ `time` \u0914\u0930 `calendar`."
title: "\u0924\u093E\u0930\u0940\u0916 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\
  \u093F\u0902\u0917 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u093E"
weight: 28
---

## तरीका:
```Haskell
import Data.Time

-- वर्तमान समय व तारीख प्राप्त करना
main :: IO ()
main = do
    currentTime <- getCurrentTime
    let currentDay = utctDay currentTime
    print $ "Default format: " ++ show currentDay
    -- विशिष्ट तारीख सरणी में बदलना
    let dateString = formatTime defaultTimeLocale "%d-%m-%Y" currentDay
    putStrLn $ "DD-MM-YYYY format: " ++ dateString
```
सैंपल आउटपुट:
```
"Default format: 2023-03-25" 
"DD-MM-YYYY format: 25-03-2023"
```

## गहराई में जानकारी:
तारीख को स्ट्रिंग में बदलने की प्रक्रिया को `Data.Time` लाइब्रेरी में `formatTime` फंक्शन द्वारा सम्भव किया गया है, जो Haskell के पूर्व वर्ज़न्स से शामिल है। यह प्रोग्रामर को विभिन्न तारीख स्वरूपों (formats) को चुनने का विकल्प देता है, जैसे कि दिन-महीना-वर्ष (`"%d-%m-%Y"`) या महीना-दिन-वर्ष (`"%m/%d/%Y"`)। विकल्प के तौर पर, अन्य हाई-लेवल डेट्स हैंडलिंग लाइब्रेरिज़ भी होती हैं, जैसे `time` और `calendar`.

`formatTime` का प्रयोग करने से पहले, `currentTime` फंक्शन `IO` अभिकलन में वर्तमान UTC समय और तारीख प्राप्त करता है, जिसे बाद में स्ट्रिंग में बदला जा सकता है। चूँकि Haskell एक मजबूत टाइम की स्टैटिक टाइपिंग भाषा है, `show` फंक्शन का प्रयोग डिफ़ॉल्ट स्ट्रिंग नरेशन के तौर पर होता है।

## सी अल्सो:
- Haskell `Data.Time` लाइब्रेरी का डॉक्युमेंटेशन: [Hackage Data.Time Library](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- `formatTime` फंक्शन: [FormatTime Function](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html#v:formatTime)

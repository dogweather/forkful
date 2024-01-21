---
title:                "तारीख को स्ट्रिंग में बदलना"
date:                  2024-01-20T17:37:37.972299-07:00
model:                 gpt-4-1106-preview
simple_title:         "तारीख को स्ट्रिंग में बदलना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

तारीख को स्ट्रिंग में बदलना (converting a date into a string) से आशय है तारीख के डेटा को पठनीय पाठ फॉर्मेट में परिवर्तित करना। प्रोग्रामर्स इसे इसलिए करते हैं क्योंकि इससे तारीख को यूज़र-फ्रेंडली तरीके से डिस्प्ले किया जा सकता है और लॉग फाइल्स, मैसेजेस एवं अन्य डॉक्यूमेंटेशन्स में आसानी से समझा जा सकता है। 

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
---
title:                "वर्तमान तिथि प्राप्त करना"
html_title:           "Haskell: वर्तमान तिथि प्राप्त करना"
simple_title:         "वर्तमान तिथि प्राप्त करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्यों

अगर आप किसी भी समय जानना चाहते हैं कि वर्तमान दिनांक क्या है, तो आपको उसके लिए हास्केल प्रोग्रामिंग भाषा का उपयोग करके स्वयं इसका पता लगाने की ताकत है।

## कैसे करें

आप निम्नलिखित तरीके से देख सकते हैं कि हास्केल में वर्तमान दिनांक कैसे प्राप्त किया जाता है:

```Haskell
import Data.Time.Clock
import Data.Time.LocalTime

getCurrentDate :: IO ()
getCurrentDate = do
  time <- getCurrentTime
  timezone <- getCurrentTimeZone
  let localTime = utcToLocalTime timezone time
  let currentDate = localDay localTime
  putStrLn $ "Today's date is: " ++ show currentDate
```

आपको यहां `Data.Time.Clock` और `Data.Time.LocalTime` मॉड्यूल की आवश्यकता होगी। यहां हम `getCurrentTime` और `getCurrentTimeZone` फ़ंक्शन का उपयोग करके वर्तमान समय और स्थानिक समय क्षेत्र को प्राप्त करते हैं। फिर हम `utcToLocalTime` फ़ंक्शन का उपयोग करके स्थानिक समय में दिनांक और समय को प्राप्त करते हैं। अंत में हम `putStrLn` फ़ंक्शन का उपयोग करके अपने परिणाम को प्रिंट करते हैं।

```Haskell
Today's date is: 2021-10-13
```

## गहराई में जाएं

अगर आप चाहते हैं तो आप `utcToLocalTime` फ़ंक्शन का संदर्भ देकर प्राप्त की गई सभी तारीखों और समय को साथ मिलाएं सकते हैं। आप समय को अलग-अलग समय क्षेत्रों में भी प्राप्त कर सकते हैं। आप `localTimeOfDay` फ़ंक्शन का भी उपयोग कर सकते हैं जो आपको समय की गणना करने में सहायता करेगा।

## देखें भी

- [Haskell - Getting Current Time and Date](https://stackoverflow.com/questions/43225423/haskell-getting-current-time-and-date)
- [Hackage - Data.Time.Clock](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Clock.html)
- [Hackage - Data.Time.LocalTime](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-LocalTime.html)
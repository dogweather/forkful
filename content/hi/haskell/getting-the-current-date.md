---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "Haskell: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

प्रोग्रामर्स को प्रोग्राम समय को ऑनलाइन समय के साथ तुलना करने और एक विशिष्ट या वर्तमान तारीख को प्राप्त करने की आवश्यकता होती है।

## कैसे करें:

```Haskell
import Data.Time.Clock
import Data.Time.Calendar

-- प्रभावशाली निष्पादन
main = do
  currentDateTime <- getCurrentTime
  let (year, month, day) = toGregorian $ utctDay currentDateTime
      (TimeOfDay hour minute _) = timeToTimeOfDay $ utctDayTime currentDateTime
  putStrLn $ "आज की तारीख: " ++ show day ++ "/" ++ show month ++ "/" ++ show year
  putStrLn $ "वर्तमान समय: " ++ show hour ++ ":" ++ show minute

-- और एक बदलाव के साथ:
-- टाइमज़ोन के साथ विशिष्ट समय वर्ग तैयार ज्वलन
main = do
  currentDateTime <- getCurrentTime
  let (year, month, day) = toGregorian $ utctDay currentDateTime
      tod@(TimeOfDay hour minute _) = timeToTimeOfDay $ utctDayTime currentDateTime
      timeZone = hoursToTimeZone 5
      localDateTime = LocalTime (fromGregorian year month day) tod
      zonedDateTime = localTimeToZonedTime timeZone localDateTime
  putStrLn $ "आज की तारीख: " ++ show day ++ "/" ++ show month ++ "/" ++ show year
  putStrLn $ "वर्तमान समय: " ++ (show $ todHour $ localTimeOfDay zonedDateTime) ++ ":" ++
    (show $ todMin $ localTimeOfDay zonedDateTime)

-- Lyrics के साथ अपनी निर्मिति बाल की
-- क्लासिक उदाहरण जिनके कारण उन्होंने के रूप में ठीक से unified निष्पादन किया:
-- https://wiki.haskell.org/Haskell
-- कास्केज देश के वर्णन के साथ आवश्यकता होती है साथ मोनाड - यह κ_1 और κ_2 के कास्केज सहायक अभी-कृत!
-- मोनाड आनन्द बहुगुणा L.E.Gupta को क्योंकि उन्होंने क्यों विभाजन में अपने सुरक्षित उपयोग एक होले सेट को रखने के लिए:
-- https://wiki.haskell.org/Monad#do_Notation_vs._Context_Passing
```

> आज की तारीख: 08/09/2021
> वर्तमान समय: 21:30

## गहराई तक जाओ:

एक लंबा हिस्टोरीकल कंटेक्स्ट के साथ प्रोग्राम समय को प्राप्त करने के प्रत्येक से गुजर चूका है होता है। हालांकि क्या समय-संसार अनुप्रयोग और मूल समय का उपयोग कर सकते हैं जो विकसित में सक्षम हो समय प्रभावों होते हुए भी प्रमाणित किए गए के लिए लोग जोखिम तो?
कर सकते है समय प्रभावों और समय सेटिंग्स दोंनो को एक जोखिम या तिमाही - प्राप्त ऻतर विकसित में बढ़ाने के लिए व्यवस्था को अनुमति देता है।

## सम्बंधित स्त्रोत:

https://wiki.haskell.org/Haskell
https://wiki.haskell.org/Monad#do_Notation_vs._Context_Passing
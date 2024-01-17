---
title:                "भविष्य या भूतकाल में एक दिन की गणना"
html_title:           "Haskell: भविष्य या भूतकाल में एक दिन की गणना"
simple_title:         "भविष्य या भूतकाल में एक दिन की गणना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
पुरानी तारीख को भविष्य में या भविष्य की तारीख को पिछले दिनों में हिसाब लगाना एक काम है। इसे करने का कारण है कि कई प्रोग्रामिंग कार्यों में तारीखों का उपयोग आवश्यक होता है।

## कैसे करें:
Haskell में एक तारीख को भविष्य में या पिछले दिनों में हिसाब लगाने के लिए, आप ```addDays``` फंक्शन का उपयोग कर सकते हैं। यह आपको दिनों का संख्यात्मक अंतर देकर अपनी पहली तारीख में से एक नई तारीख उत्पन्न करता है। नीचे दिए गए उदाहरण में, हम 10 दिन बाद की तारीख निकालते हैं और इसका परिणाम दिखाते हैं:
```Haskell
import Data.Time.Clock
import Data.Time.Calendar
today <- getCurrentTime
let tenDaysFromNow = addDays 10 (utctDay today)
print tenDaysFromNow
```
उत्पाद:
2020-04-24

## गहराई में:
तारीख को भविष्य में या पिछले दिनों में हिसाब लगाना इतिहास में बहुत ही पुरानी एक प्रक्रिया है। प्राचीन समय में, लोग चंद्रमा या सूर्य के उदय और अस्त होने को तारीख के रूप में उपयोग करते थे। अब ईंधन और शक्ति के क्षेत्र में तेजी से विकास के साथ, लोग समय को सटीक होने के लिए नए तरीके खोज रहे हैं। आजकल, इसे अलग-अलग कारणों के लिए एक साफ़ और सटीक तरीके से किया जा सकता है। अन्य तकनीकियों में इसे करने के लिए, पूर्व और भविष्यवाणी तारीख को देखने के लिए बड़े संख्यात्मक लेखन का उपयोग किया जाता है।

## इससे जुड़े लिंक:
समय और तारीख को हिसाब लगाने के लिए और भी तकनीकों के बारे में अधिक जानकारी के लिए, इन लिंक को देखें:
- https://www.geeksforgeeks.org/haskell-date-manipulation/
- https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Clock.html
- https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-LocalTime.html
- https://en.wikipedia.org/wiki/Date_calculation_in_software
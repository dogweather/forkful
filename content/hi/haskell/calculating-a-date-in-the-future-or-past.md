---
title:                "भविष्य या अतीत में एक तारीख की गणना"
html_title:           "Haskell: भविष्य या अतीत में एक तारीख की गणना"
simple_title:         "भविष्य या अतीत में एक तारीख की गणना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

तारिख की गणना भविष्य या अतीत में होने के विशेष स्थिति का पता लगाने की क्रिया होती है। प्रोग्रामर इसे उत्कृष्ट योजना बनाने के लिए करते हैं, खासकर जब कार्यक्रमों, म्यूअलेटर्स या विष्लेषिकाओं को समयावधि के साथ काम करना होता है।

## कैसे:

```Haskell
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate

-- तारीख की गणना करने के फ़ंक्शन
futureDate :: Day -> Integer -> Day
futureDate date days = addDays days date

-- उदाहरण
let date = fromGregorian 2019 2 12 
let future = futureDate date 365
```
आउटपुट:
```Haskell
2020-02-11
```

## गहरी डाइव

1. ऐतिहासिक संदर्भ: तारीखों की गणना की आवश्यकता संगणकों के जन्म से पहले थी, और यह एक महत्वपूर्ण अनुप्रयोग है जिसे अब कंप्यूटर प्रोग्रामिंग में संगणित किया जा सकता है। 

2. विकल्प: Haskell के अलावा भी अन्य कंप्यूटर भाषाएं जैसे की Java, Python, Javascript और बहुत सारी और, तारीखों की गणना करने का समर्थन करती हैं। 

3. कार्यान्वयन विवरण: `addDays` फ़ंक्शन `Data.Time.Calendar` मॉड्यूल में पाया जाता है, जिसे Haskell में दिनांक संगणना और मनिपुलेशन के लिए विशेष रूप से बनाया गया है। 

## यदि आप और जानना चाहते हैं: 

1. [Haskell डेट और टाइम का आधिकारिक डॉक्यूमेंटेशन](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Calendar.html)
2. ["समय" लाइब्रेरी के लिए Haskell प्रलेखन](https://hackage.haskell.org/package/time-1.9.3)
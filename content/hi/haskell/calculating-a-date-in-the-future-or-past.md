---
title:                "Haskell: भविष्य या भूतकाल में एक दिनांक की गणना करना"
simple_title:         "भविष्य या भूतकाल में एक दिनांक की गणना करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# क्यों

कभी-कभी हमे आने वाले या बीते हुए विशेष अवसरों की तारीखों को पता करने की जरूरत पड़ती है। हास्केल में तारीखों को निर्दिष्ट तारीखों पर आधारित करके इसका प्रोग्रामिंग किया जा सकता है। अगली या पिछली तारीखों को पता करने के उपयोग से हम अपने कोड को और उसके अनुकूलन को सुधार सकते हैं।

## कैसे करें

```Haskell
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate

-- आने वाले 10 दिनों के बाद की तारीख की प्रिंट करें
main = do
    let today = fromGregorian 2021 07 20
        futureDate = addDays 10 today
    print futureDate
```

उपरोक्त कोड के आउटपुट में हमें 2021 में जुलाई के 30 तारीख की जानकारी मिलेगी। इसी तरह हम पिछली तारीखों को भी पता कर सकते हैं। यदि हमें किसी विशेष दिन की तारीख जाननी हो तो हम [Data.Time.Calendar.OrdinalDate](https://hackage.haskell.org/package/time/docs/Data-Time-Calendar-OrdinalDate.html) मॉड्यूल का उपयोग कर सकते हैं।

## गहराई में जाएं

अगर हम हास्केल में तारीख की गणित करने के बारे में गहराई से जानते हैं तो हम अपने कोड को और भी बेहतर ढंग से लिख सकते हैं। हम [Data.Time.Calendar](https://hackage.haskell.org/package/time/docs/Data-Time-Calendar.html) और [Data.Time.Calendar.Types](https://hackage.haskell.org/package/time/docs/Data-Time-Calendar-Types.html) मॉड्यूल के साथ तारीखों की प्रकार और वर्गीकरण, तारीखों की रैंकिंग, और अन्य सम्बंधित विषयों पर अधिक जानकारी प्राप्त कर सकते हैं।

# इस से जुड़े

[हास्केल में तारीखों की गणित कैसे करें](https://www.tutorialspoint.com/haskell/haskell_date_time.htm)

[Haskell में तारीखों को प्रसं
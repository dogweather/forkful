---
title:                "भविष्य या अतीत में तारीख की गणना"
aliases: - /hi/haskell/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:42.290069-07:00
model:                 gpt-4-1106-preview
simple_title:         "भविष्य या अतीत में तारीख की गणना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
भविष्य या अतीत की तारीख की गणना का मतलब है किसी तिथि से आगे या पीछे की तिथि निकालना। प्रोग्रामर इसे इवेंट प्लानिंग, डेडलाइन ट्रैकिंग और डाटा आर्काइविंग में इस्तेमाल करते हैं।

## कैसे करें? (How to:)
```Haskell
import Data.Time

-- आज की तारीख और भविष्य/अतीत की तारीख निकालें
main = do
    today <- getCurrentTime
    let thirtyDaysLater = addDays 30 today
    let thirtyDaysBefore = addDays (-30) today
    putStrLn $ "आज की तारीख: " ++ show today
    putStrLn $ "30 दिन बाद की तारीख: " ++ show thirtyDaysLater
    putStrLn $ "30 दिन पहले की तारीख: " ++ show thirtyDaysBefore
```

सैंपल आउटपुट:
```
आज की तारीख: 2023-04-01 12:00:00 UTC
30 दिन बाद की तारीख: 2023-05-01 12:00:00 UTC
30 दिन पहले की तारीख: 2023-03-02 12:00:00 UTC
```

## गहन जानकारी (Deep Dive)
'डेटा.टाइम' लाइब्रेरी में 'addDays' फंक्शन हास्केल का एक पावरफुल टूल है जो डेट कैलकुलेशन को आसान बनाता है। इससे पहले, डेवलपर्स को मैनुअली साल, महीने, और दिन की गणना करनी पड़ती थी, जिससे भूलचूक की संभावनाएं बढ़ जाती थीं। 'डेटा.टाइम' के अलावा, दूसरे पैकेज जैसे 'time' और 'chronos' भी हैं, लेकिन 'डेटा.टाइम' सबसे ज्यादा इस्तेमाल किया जाता है। 

'addDays' में सिर्फ दिनों के लिए फंक्शन होता है, अगर हमें महीनों या सालों को जोड़ना होता है तो 'addGregorianMonthsClip' या 'addGregorianYearsClip' जैसे फंक्शन्स का उपयोग होता है।

## और भी जानकारी (See Also)
- Haskell Documentation for `Data.Time` library: [Data.Time](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- Haskell Library 'time': [time on hackage](https://hackage.haskell.org/package/time)

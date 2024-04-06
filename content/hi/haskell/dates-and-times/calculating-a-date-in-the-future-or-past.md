---
date: 2024-01-20 17:31:42.290069-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902? (How to:) \u0938\u0948\
  \u0902\u092A\u0932 \u0906\u0909\u091F\u092A\u0941\u091F."
lastmod: '2024-04-05T21:53:54.417724-06:00'
model: gpt-4-1106-preview
summary: "(How to:) \u0938\u0948\u0902\u092A\u0932 \u0906\u0909\u091F\u092A\u0941\u091F\
  ."
title: "\u092D\u0935\u093F\u0937\u094D\u092F \u092F\u093E \u0905\u0924\u0940\u0924\
  \ \u092E\u0947\u0902 \u0924\u093E\u0930\u0940\u0916 \u0915\u0940 \u0917\u0923\u0928\
  \u093E"
weight: 26
---

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

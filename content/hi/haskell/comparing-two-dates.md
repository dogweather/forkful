---
title:                "दो तारीखों की तुलना"
date:                  2024-01-20T17:33:28.807651-07:00
model:                 gpt-4-1106-preview
simple_title:         "दो तारीखों की तुलना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
तारीखों की तुलना करने का मतलब है दो तिथियों को आपस में मिलान करना। प्रोग्रामर्स यह काम तिथियों की सीमाओं को जानने, समय-सीमा की पुष्टि करने, और क्रमबद्ध करने के लिए करते हैं।

## How to: (कैसे करें:)
```Haskell
import Data.Time

-- दो तिथियों की तुलना करने का फंक्शन
compareDates :: Day -> Day -> Ordering
compareDates date1 date2 = compare date1 date2

main :: IO ()
main = do
    -- तिथियों का उदाहरण
    let date1 = fromGregorian 2023 1 1 -- 1 जनवरी 2023
    let date2 = fromGregorian 2023 12 31 -- 31 दिसंबर 2023
    
    -- तुलना का परिणाम प्रिंट करना
    print $ compareDates date1 date2
```
आउटपुट:
```Haskell
LT -- 'LT' का मतलब है date1 कम है date2 से (Less Than)
```

## Deep Dive (गहराई से जानकारी):
तिथियों की तुलना ऐतिहासिक रूप से कैलेंडर सिस्टमों के विकास के साथ आई है। Haskell `Data.Time` लाइब्रेरी में इसे कार्यान्वित करने के कई तरीके हैं, जैसे कि `UTCTime`, `ZonedTime`, आदि उपयोग करना। `compareDates` फंक्शन `compare` का इस्तेमाल कर के `Day` टाइप की तिथियों की स्वतः तुलना करता है। आप `before`, `after` जैसे फंक्शन्स का भी इस्तेमाल कर सकते हैं।

## See Also (और जानकारी के लिए):
- Haskell Documentation for `Data.Time` Library: [https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)

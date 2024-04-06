---
date: 2024-01-20 17:33:28.807651-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0924\u093F\
  \u0925\u093F\u092F\u094B\u0902 \u0915\u0940 \u0924\u0941\u0932\u0928\u093E \u0910\
  \u0924\u093F\u0939\u093E\u0938\u093F\u0915 \u0930\u0942\u092A \u0938\u0947 \u0915\
  \u0948\u0932\u0947\u0902\u0921\u0930 \u0938\u093F\u0938\u094D\u091F\u092E\u094B\u0902\
  \ \u0915\u0947 \u0935\u093F\u0915\u093E\u0938 \u0915\u0947 \u0938\u093E\u0925 \u0906\
  \u0908 \u0939\u0948\u0964 Haskell `Data.Time` \u0932\u093E\u0907\u092C\u094D\u0930\
  \u0947\u0930\u0940 \u092E\u0947\u0902 \u0907\u0938\u0947 \u0915\u093E\u0930\u094D\
  \u092F\u093E\u0928\u094D\u0935\u093F\u0924 \u0915\u0930\u0928\u0947 \u0915\u0947\
  \ \u0915\u0908\u2026"
lastmod: '2024-04-05T22:51:07.112264-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0924\u093F\u0925\u093F\
  \u092F\u094B\u0902 \u0915\u0940 \u0924\u0941\u0932\u0928\u093E \u0910\u0924\u093F\
  \u0939\u093E\u0938\u093F\u0915 \u0930\u0942\u092A \u0938\u0947 \u0915\u0948\u0932\
  \u0947\u0902\u0921\u0930 \u0938\u093F\u0938\u094D\u091F\u092E\u094B\u0902 \u0915\
  \u0947 \u0935\u093F\u0915\u093E\u0938 \u0915\u0947 \u0938\u093E\u0925 \u0906\u0908\
  \ \u0939\u0948\u0964 Haskell `Data.Time` \u0932\u093E\u0907\u092C\u094D\u0930\u0947\
  \u0930\u0940 \u092E\u0947\u0902 \u0907\u0938\u0947 \u0915\u093E\u0930\u094D\u092F\
  \u093E\u0928\u094D\u0935\u093F\u0924 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0915\
  \u0908 \u0924\u0930\u0940\u0915\u0947 \u0939\u0948\u0902, \u091C\u0948\u0938\u0947\
  \ \u0915\u093F `UTCTime`, `ZonedTime`, \u0906\u0926\u093F \u0909\u092A\u092F\u094B\
  \u0917 \u0915\u0930\u0928\u093E\u0964 `compareDates` \u092B\u0902\u0915\u094D\u0936\
  \u0928 `compare` \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\
  \u0930 \u0915\u0947 `Day` \u091F\u093E\u0907\u092A \u0915\u0940 \u0924\u093F\u0925\
  \u093F\u092F\u094B\u0902 \u0915\u0940 \u0938\u094D\u0935\u0924\u0903 \u0924\u0941\
  \u0932\u0928\u093E \u0915\u0930\u0924\u093E \u0939\u0948\u0964 \u0906\u092A `before`,\
  \ `after` \u091C\u0948\u0938\u0947 \u092B\u0902\u0915\u094D\u0936\u0928\u094D\u0938\
  \ \u0915\u093E \u092D\u0940 \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\
  \u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964."
title: "\u0926\u094B \u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u0940 \u0924\
  \u0941\u0932\u0928\u093E"
weight: 27
---

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

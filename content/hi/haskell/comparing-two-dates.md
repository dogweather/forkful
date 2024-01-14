---
title:                "Haskell: दो तारीखों की तुलना करना"
simple_title:         "दो तारीखों की तुलना करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्यों
एक आम चुनौती जो किसी भी प्रोग्रामर को सामना करना पड़ता है, वह है दो तारीखों की तुलना करना। हास्केल में दो तारीखों को तुलना करना एक आसान कार्य हो सकता है लेकिन अन्य प्रोग्रामिंग भाषाओं की तुलना में इसमें थोड़ी चुनौती हो सकती है। इस लेख में हम हिंदी भाषा में हास्केल के माध्यम से दो तारीखों की तुलना करने के बारे में बात करेंगे।

## कैसे करें
तारीखों की तुलना करने के लिए हमें दो तारीखों को `Day`, `Month` और `Year` के निर्देशकों में अलग करने की आवश्यकता होती है। हास्केल में `Data.Time` लाइब्रेरी में दो तारीखों को तुलना करने के लिए `compare` फ़ंक्शन होता है। एक सरल उदाहरण के साथ, हम इसका उपयोग कर सकते हैं:

```Haskell
import Data.Time

-- संभव रूप से सबसे कम युगन्तर तिथि का मान लाय।
minimumDate :: Day
minimumDate = fromGregorian 0 1 1

-- दो तारीखों को तुलना करें और उनमें से बड़े को लाय।
compareDates :: Day -> Day -> Ordering
compareDates date1 date2 = compare date1 date2

-- उपयोगकर्ता को दोतारीखों के बीच का फर्क दिखाए।
main = do
  let date1 = fromGregorian 2021 5 21
  let date2 = fromGregorian 2021 5 22
  print $ compareDates date1 date2
```

आउटपुट:
```
LT
```

इस उदाहरण में, हमने दो तारीखों को तुलना करके दोनों के बीच फ़र्क को दिखाया है। यहां, `LT` आउटपुट है जो दर्शाता है कि `date1` द्वारा प्रदत्त तारीख `date2` से छोटी है।

## गहराई में जाएं
हास्केल क
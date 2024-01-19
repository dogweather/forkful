---
title:                "दो तारीखों की तुलना"
html_title:           "Elixir: दो तारीखों की तुलना"
simple_title:         "दो तारीखों की तुलना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
दो तारीखों की तुलना करना मतलब है यह सुनिश्चित करना कि कौनसी तारीख पहले या बाद में है, या क्या वे एक ही हैं। प्रोग्रामर इसे कार्यों को अनुसूचित करने, समय-संबंधी नियमों का पालन करने आदि के लिए करते हैं।

## कैसे करें:
में, हम एक `Data.Time` मॉड्यूल का उपयोग करके तारीखों की तुलना करेंगे।

```Haskell
import Data.Time

day1 :: Day
day1 = fromGregorian 2000 5 15

day2 :: Day
day2 = fromGregorian 2000 7 25

compareDates = compare day1 day2
```

इसका उत्तर होगा `LT` यानी `day1` `day2` से पहले है।

## गहरी डाइव
तारीखों की तुलना में हम आमतौर पर `Data.Time` मॉड्यूल का उपयोग करते हैं, जिसका विकास काफी पुराना है और यह Haskell की मूल रूप से क्षुद्र कार्यक्षमता का हिस्सा है। वैकल्पिक रूप से, आप कस्टम तुलना ऑपरेटर भी बना सकते हैं, लेकिन `Data.Time` ने इस क्षेत्र में जरूरी कार्यवाही की है। तुलना करने के लिए, यह `Ord` टाइप क्लास का उपयोग करता है, जिसमें `compare` मेथड शामिल होता है।

## अन्य फ़टाफट देखें
अगर आप गहराई में जाना चाहते हैं, तो निम्नलिखित स्रोत देखें:
- [Haskell's Data.Time Module](https://hackage.haskell.org/package/time-1.11.1.1/docs/Data-Time.html)
- [Haskell's Ord Typeclass](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#t:Ord)
- [Haskell Date Manipulation](https://riptutorial.com/haskell/example/5804/date-manipulation)
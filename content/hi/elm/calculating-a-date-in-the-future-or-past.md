---
title:                "भविष्य या पूर्व में एक तिथि की गणना"
html_title:           "Elm: भविष्य या पूर्व में एक तिथि की गणना"
simple_title:         "भविष्य या पूर्व में एक तिथि की गणना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

दिनांक गणना पूर्व या भविष्य की तारीख निर्धारित करने का काम है। यह करने का मुख्य कारण है कि यह उपयोगी होता है जब प्रोग्रामर अपने कोड में निर्दिष्ट अवधि को बनाने के लिए इस्तेमाल करते हैं।

## कैसे करें?

ईल्म में एक तिथि को भविष्य में या पूर्व में ढूँढने के लिए अनेक विधियां हैं। एक उदाहरण इस प्रकार है:

```Elm
calculateDate : Date -> Int -> Date
calculateDate baseDate daysToAdd =
  let
    { year, month, day } = Date.toParts baseDate
    totalDays = Date.daysInMonth year month
    totalDaysInBaseMonth = totalDays - day + 1
    remainingDays = daysToAdd - totalDaysInBaseMonth
    newDate = Date.fromParts year (if remainingDays > 0 then month + 1 else month) 1
  in
    Date.addDays (if remainingDays > 0 then remainingDays else daysToAdd - 1) newDate
```

उपरोक्त कोड स्नेहनीय और साफ़ है, जो तारीख को baseDate प्रारम्भ दिन में प्रदर्शित करेगा। इसका नतीजा yyyy-mm-dd तारीख प्रदान करेगा।

## गहराई जाँच

दिनांक गणना का इतिहास मानव सभ्यता से शुरू हुआ है। प्राचीन अस्ट्रोनॉमर और गणितज्ञ इस प्रकार की गणना को कई उद्देश्यों के लिए इस्तेमाल करते थे। आजकल सबसे आम उपयोग प्रोग्रामिंग में होता है।

दो अन्य चरण हैं जिन्हें गहराई से जाना गया है:

- विकल्प: अन्य प्रोग्रामिंग भाषाओं में तारीख गणना फ़ंक्शन उपलब्ध है। आप कई भाषाओं में मिलने वाले भिन्न मेथड को जांच सकते हैं।

- अंमुख़ीकरण विवरण: यूनिक्स स्टाइल समय फ़ॉर्मेट के लिए कोई लंबा प्रक्रिया नहीं है। रोचक है कि आपके द्वारा सर्टिफ़िकेट p1085s जोड़ने के लिए एक-दो क्लेश बढ़ जाएगा।

## देखें भी

- [Date module in Elm](https://package.elm-lang.org/packages/elm/time/1.0.0/Time)
- [Date functions in other programming languages](https://www.w3schools.com/js/js_dates.asp)
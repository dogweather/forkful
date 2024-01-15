---
title:                "भविष्य या भूतकाल में तारीख की गणना"
html_title:           "Java: भविष्य या भूतकाल में तारीख की गणना"
simple_title:         "भविष्य या भूतकाल में तारीख की गणना"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्यों

दिनांकों को आगामी या अतीत में गिनने के लिए कोडिंग करने का काम किसी को पुराने बोरिंग से सर्वोत्तम बनाने में मदद कर सकता है।

## कैसे करें

आप आगामी या अतीत में एक नया दिनांक जानना चाहते हैं तो निम्नलिखित तरीकों को अपनाएं:

```Java
// वर्तमान तारीख ले लो
LocalDate currentDate = LocalDate.now();

// स्कालर वर्ष जोड़े या घटाएं (गणना को आगे या पीछे ले जाने के लिए)
LocalDate dateInFuture = currentDate.plusYears(1);
LocalDate dateInPast = currentDate.minusYears(2);

// दिन जोड़ें या घटाएं (गणना को आगे या पीछे ले जाने के लिए)
LocalDate dateInFuture = currentDate.plusDays(30);
LocalDate dateInPast = currentDate.minusDays(15);

// महीनों जोड़ें या घटाएं (गणना को आगे या पीछे ले जाने के लिए)
LocalDate dateInFuture = currentDate.plusMonths(6);
LocalDate dateInPast = currentDate.minusMonths(3);

// साल जोड़ें या घटाएं (गणना को आगे या पीछे ले जाने के लिए)
LocalDate dateInFuture = currentDate.plusYears(5);
LocalDate dateInPast = currentDate.minusYears(10);
```
आउटपुट:
```2022-06-15
2018-06-15
2021-07-15
2018-04-15
2020-12-15
2018-03-15
2024-06-15
2008-06-15
```

## गहराई में

Java में दिनांकों को आगामी या अतीत में कैसे गणना किया जाए, इसके लिए `java.time` पैकेज का उपयोग किया जाता है। `LocalDate` ऑब्जेक्ट दिनांक को रखने के लिए एक अच्छा आयाम है और `plusYears()`, `plusDays()`, `plusMonths()`, और `minusYears()`, `minusDays()`, `minusMonths()` में से कोई भी उपकरण नया दिनांक बनाने के लिए हो सकता है।

## भी देखें

- [Java के डेट ऑब्जेक्ट का उपयोग करना](https://www.geeksforgeeks.org/date-class-useful-methods-java/)
- [Java में समय का प्र
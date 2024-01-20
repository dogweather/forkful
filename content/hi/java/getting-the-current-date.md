---
title:                "वर्तमान तारीख प्राप्त करना"
date:                  2024-01-20T15:15:25.450640-07:00
html_title:           "C: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
जावा में मौजूदा तारीख पाना यानी सिस्टम की तारीख को प्राप्त करना होता है। प्रोग्रामर्स यह इसलिए करते हैं ताकि वे टाइमस्टैम्प बना सकें, यूजर्स को ग्रीटिंग्स दिखा सकें या रिपोर्ट्स में डेट डाल सकें।

## How to: (कैसे करें:)
```java
import java.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate today = LocalDate.now();
        System.out.println("आज की तारीख: " + today);
    }
}
```
उदाहरण आउटपुट:
```
आज की तारीख: 2023-03-31
```

## Deep Dive (गहन जानकारी)
जावा में तारीख को प्राप्त करने के लिए `java.util.Date`, `java.util.Calendar` और `java.time.LocalDate` जैसी कई क्लासेस हैं। पहले `Date` और `Calendar` का इस्तेमाल ज्यादा था, पर Java 8 के बाद से `java.time` पैकेज पसंदीदा हो गया, क्योंकि इसमें immutable classes और बेहतर API design हैं। `LocalDate.now()` का इस्तेमाल करना सबसे आसान और सीधा है। `java.time.ZonedDateTime` या `java.time.LocalDateTime` का उपयोग टाइमज़ोन के साथ तारीख और समय पाने के लिए किया जा सकता है।

## See Also (अन्य सूत्र)
- [Java API Documentation for LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Java Date and Time - Baeldung](https://www.baeldung.com/java-8-date-time-intro)
- [Java 8 Date Time API - JournalDev](https://www.journaldev.com/2800/java-8-date-localdate-localdatetime-instant)
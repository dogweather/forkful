---
title:                "तारीख को स्ट्रिंग में बदलना"
aliases: - /hi/java/converting-a-date-into-a-string.md
date:                  2024-01-20T17:37:24.983482-07:00
model:                 gpt-4-1106-preview
simple_title:         "तारीख को स्ट्रिंग में बदलना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
जावा में तारीख को स्ट्रिंग में बदलने का मतलब है एक डेट ऑब्जेक्ट को पढ़ने योग्य फॉर्मेट में परिवर्तित करना। प्रोग्रामर इसे लॉग्स, यूजर इंटरफ़ेस, या डेटा एक्सचेंज के लिए करते हैं।

## How to: (कैसे करें:)
```java
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

public class DateToStringExample {
    public static void main(String[] args) {
        LocalDateTime now = LocalDateTime.now();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss");
        String formattedDate = now.format(formatter);
        System.out.println(formattedDate);
    }
}
```
उदाहरण आउटपुट:
```
31-03-2023 20:45:30
```

## Deep Dive (गहराई से जानकारी):
तारीख को स्ट्रिंग में बदलना जावा के पुराने वर्ज़न में `SimpleDateFormat` के साथ होता था। `java.time` पैकेज, जिसे JSR 310 कहते हैं, जावा 8 में आया, जो ज्यादा आसान और त्रुटिरहित है। `DateTimeFormatter` का इस्तेमाल करके अलग अलग फॉर्मेट पैटर्न बनाए जा सकते हैं। `LocalDateTime`, `LocalDate` और `LocalTime` जैसे क्लासेज के साथ इस्तेमाल होता है। `ZonedDateTime` का इस्तेमाल करने से टाइमज़ोन के साथ तारीख और समय संभाला जा सकता है।

## See Also (और भी देखें):
- [DateTimeFormatter JavaDoc](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Java Platform Standard Ed. 8 Date and Time Guide](https://docs.oracle.com/javase/tutorial/datetime/)
- [Java SimpleDateFormat](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)

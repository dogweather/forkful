---
date: 2024-01-20 17:37:24.983482-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0924\u093E\
  \u0930\u0940\u0916 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\
  \ \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u093E \u091C\u093E\u0935\u093E \u0915\
  \u0947 \u092A\u0941\u0930\u093E\u0928\u0947 \u0935\u0930\u094D\u095B\u0928 \u092E\
  \u0947\u0902 `SimpleDateFormat` \u0915\u0947 \u0938\u093E\u0925 \u0939\u094B\u0924\
  \u093E \u0925\u093E\u0964 `java.time` \u092A\u0948\u0915\u0947\u091C, \u091C\u093F\
  \u0938\u0947 JSR 310 \u0915\u0939\u0924\u0947 \u0939\u0948\u0902,\u2026"
lastmod: '2024-04-05T22:51:06.815199-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0924\u093E\u0930\u0940\
  \u0916 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u092E\u0947\
  \u0902 \u092C\u0926\u0932\u0928\u093E \u091C\u093E\u0935\u093E \u0915\u0947 \u092A\
  \u0941\u0930\u093E\u0928\u0947 \u0935\u0930\u094D\u095B\u0928 \u092E\u0947\u0902\
  \ `SimpleDateFormat` \u0915\u0947 \u0938\u093E\u0925 \u0939\u094B\u0924\u093E \u0925\
  \u093E\u0964 `java.time` \u092A\u0948\u0915\u0947\u091C, \u091C\u093F\u0938\u0947\
  \ JSR 310 \u0915\u0939\u0924\u0947 \u0939\u0948\u0902, \u091C\u093E\u0935\u093E\
  \ 8 \u092E\u0947\u0902 \u0906\u092F\u093E, \u091C\u094B \u091C\u094D\u092F\u093E\
  \u0926\u093E \u0906\u0938\u093E\u0928 \u0914\u0930 \u0924\u094D\u0930\u0941\u091F\
  \u093F\u0930\u0939\u093F\u0924 \u0939\u0948\u0964 `DateTimeFormatter` \u0915\u093E\
  \ \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0915\u0947 \u0905\
  \u0932\u0917 \u0905\u0932\u0917 \u092B\u0949\u0930\u094D\u092E\u0947\u091F \u092A\
  \u0948\u091F\u0930\u094D\u0928 \u092C\u0928\u093E\u090F \u091C\u093E \u0938\u0915\
  \u0924\u0947 \u0939\u0948\u0902\u0964 `LocalDateTime`, `LocalDate` \u0914\u0930\
  \ `LocalTime` \u091C\u0948\u0938\u0947 \u0915\u094D\u0932\u093E\u0938\u0947\u091C\
  \ \u0915\u0947 \u0938\u093E\u0925 \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932\
  \ \u0939\u094B\u0924\u093E \u0939\u0948\u0964 `ZonedDateTime` \u0915\u093E \u0907\
  \u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0928\u0947 \u0938\u0947\
  \ \u091F\u093E\u0907\u092E\u091C\u093C\u094B\u0928 \u0915\u0947 \u0938\u093E\u0925\
  \ \u0924\u093E\u0930\u0940\u0916 \u0914\u0930 \u0938\u092E\u092F \u0938\u0902\u092D\
  \u093E\u0932\u093E \u091C\u093E \u0938\u0915\u0924\u093E \u0939\u0948\u0964."
title: "\u0924\u093E\u0930\u0940\u0916 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\
  \u093F\u0902\u0917 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u093E"
weight: 28
---

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

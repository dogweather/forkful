---
date: 2024-01-20 17:37:24.983482-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0909\u0926\
  \u093E\u0939\u0930\u0923 \u0906\u0909\u091F\u092A\u0941\u091F."
lastmod: '2024-04-05T21:53:54.144848-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0909\u0926\u093E\u0939\
  \u0930\u0923 \u0906\u0909\u091F\u092A\u0941\u091F."
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

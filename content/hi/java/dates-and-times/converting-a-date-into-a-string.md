---
date: 2024-01-20 17:37:24.983482-07:00
description: "\u091C\u093E\u0935\u093E \u092E\u0947\u0902 \u0924\u093E\u0930\u0940\
  \u0916 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u092E\u0947\
  \u0902 \u092C\u0926\u0932\u0928\u0947 \u0915\u093E \u092E\u0924\u0932\u092C \u0939\
  \u0948 \u090F\u0915 \u0921\u0947\u091F \u0911\u092C\u094D\u091C\u0947\u0915\u094D\
  \u091F \u0915\u094B \u092A\u0922\u093C\u0928\u0947 \u092F\u094B\u0917\u094D\u092F\
  \ \u092B\u0949\u0930\u094D\u092E\u0947\u091F \u092E\u0947\u0902 \u092A\u0930\u093F\
  \u0935\u0930\u094D\u0924\u093F\u0924 \u0915\u0930\u0928\u093E\u0964 \u092A\u094D\
  \u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\u0947 \u0932\u0949\
  \u0917\u094D\u0938, \u092F\u0942\u091C\u0930 \u0907\u0902\u091F\u0930\u092B\u093C\
  \u0947\u0938, \u092F\u093E \u0921\u0947\u091F\u093E\u2026"
lastmod: 2024-02-19 22:05:11.132150
model: gpt-4-1106-preview
summary: "\u091C\u093E\u0935\u093E \u092E\u0947\u0902 \u0924\u093E\u0930\u0940\u0916\
  \ \u0915\u094B \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u092E\u0947\u0902\
  \ \u092C\u0926\u0932\u0928\u0947 \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u0948\
  \ \u090F\u0915 \u0921\u0947\u091F \u0911\u092C\u094D\u091C\u0947\u0915\u094D\u091F\
  \ \u0915\u094B \u092A\u0922\u093C\u0928\u0947 \u092F\u094B\u0917\u094D\u092F \u092B\
  \u0949\u0930\u094D\u092E\u0947\u091F \u092E\u0947\u0902 \u092A\u0930\u093F\u0935\
  \u0930\u094D\u0924\u093F\u0924 \u0915\u0930\u0928\u093E\u0964 \u092A\u094D\u0930\
  \u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\u0947 \u0932\u0949\u0917\
  \u094D\u0938, \u092F\u0942\u091C\u0930 \u0907\u0902\u091F\u0930\u092B\u093C\u0947\
  \u0938, \u092F\u093E \u0921\u0947\u091F\u093E\u2026"
title: "\u0924\u093E\u0930\u0940\u0916 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\
  \u093F\u0902\u0917 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u093E"
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

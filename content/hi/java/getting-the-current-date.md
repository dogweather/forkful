---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "C#: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

**शीर्षक**: जावा में वर्तमान तारीख प्राप्त करना

## क्या और क्यों?

वर्तमान तारीख प्राप्त करना का अर्थ है सिस्टम के अनुसार वर्तमान तारीख और समय को प्राप्त करना। प्रोग्रामर्स के लिए यह महत्वपूर्ण हो सकता है ताकि वे कोई रिपोर्ट या लॉग फ़ाइल तैयार कर सकें।

## कैसे करें:

आइए देखते हैं कि कोड कैसे लिखा जाए।

```Java
import java.time.LocalDateTime;

public class CurrentDateExample {
   public static void main(String args[]) {
       LocalDateTime current = LocalDateTime.now();
       System.out.println("वर्तमान तारीख और समय: "+ current);
   }
}
```

उपरोक्त कोड का आउटपुट कुछ इस तरह होगा:

```
वर्तमान तारीख और समय: 2021-10-28T14:57:52.212
```

## गहरे प्रवेश:

### ऐतिहासिक प्रसंग:

Java 8 में java.time package को जोड़ा गया था। पहले डेवलपर्स java.util.Date और java.util.Calendar का उपयोग करते थे, जिनमें काफी अभियोजन थीं।

### वैकल्पिक तरीके:

इन पुराने क्लासेज का अब भी इस्तेमाल किया जा सकता है:

```Java
import java.util.Date;

public class AlternateCurrentDateExample {
   public static void main(String args[]) {
       Date current = new Date();
       System.out.println("वर्तमान तारीख और समय: "+ current);
   }
}
```

### क्रियान्वयन विवरण:

`LocalDateTime.now()` एक मशीन का वर्तमान टाइमज़ोन के अनुसार तारीख और समय वापस करता है। 

## और भी देखें:

- Oracle की Java Docs: [www.oracle.com/technetwork/java/javase/documentation/index.html](http://www.oracle.com/technetwork/java/javase/documentation/index.html)
- Java Date & Time ट्यूटोरियल्स: [www.baeldung.com/java-8-date-time-intro](http://www.baeldung.com/java-8-date-time-intro)
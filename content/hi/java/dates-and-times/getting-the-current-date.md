---
title:                "वर्तमान तारीख प्राप्त करना"
aliases: - /hi/java/getting-the-current-date.md
date:                  2024-02-03T19:10:45.592794-07:00
model:                 gpt-4-0125-preview
simple_title:         "वर्तमान तारीख प्राप्त करना"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
जावा में वर्तमान तारीख प्राप्त करना एक मूलभूत कार्य है जो प्रोग्रामरों को लॉगिंग, तारीख की गणना, और समय-आधारित शर्तों जैसे संचालनों के लिए तारीख की वस्तुओं को संभालने की अनुमति देता है। यह उन अनुप्रयोगों में महत्वपूर्ण है जहां ट्रैकिंग, अनुसूचीकरण, और समय संबंधी डेटा विश्लेषण आवश्यक हैं।

## कैसे:
जावा वर्तमान तारीख प्राप्त करने के लिए कई तरीके प्रदान करता है, पुरानी `java.util.Date` क्लास का उपयोग करते हुए और नए `java.time` पैकेज (जावा 8 में परिचय) का उपयोग करते हुए जो अधिक बहुउपयोगी और सहज है।

### `java.time.LocalDate` का उपयोग करते हुए
```java
import java.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // उदाहरण आउटपुट: 2023-04-01
    }
}
```
### `java.time.LocalDateTime` का उपयोग करते हुए
```java
import java.time.LocalDateTime;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDateTime currentDateTime = LocalDateTime.now();
        System.out.println(currentDateTime); // उदाहरण आउटपुट: 2023-04-01T12:34:56.789
    }
}
```
### `java.util.Date` (पुराना)
```java
import java.util.Date;

public class CurrentDateExample {
    public static void main(String[] args) {
        Date currentDate = new Date();
        System.out.println(currentDate); // उदाहरण आउटपुट: शनि अप्रैल 01 12:34:56 BST 2023
    }
}
```
### तृतीय-पक्ष पुस्तकालय का उपयोग करना: Joda-Time
जावा 8 से पहले, Joda-Time जावा में तारीख और समय के लिए एक प्रमुख मानक था। अगर आप पुराने सिस्टम्स पर काम कर रहे हैं या Joda-Time का प्राथमिकता देते हैं, तो यहाँ वर्तमान तारीख प्राप्त करने के लिए उसका प्रयोग कैसे करें:
```java
import org.joda.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // उदाहरण आउटपुट: 2023-04-01
    }
}
```
**नोट:** जबकि `java.util.Date` और Joda-Time अब भी प्रयोग में हैं, नए प्रोजेक्ट्स के लिए `java.time` पैकेज की सिफारिश की जाती है क्योंकि यह अविनाशी होता है और तारीख और समयों को संभालने के लिए एक व्यापक API प्रदान करता है।

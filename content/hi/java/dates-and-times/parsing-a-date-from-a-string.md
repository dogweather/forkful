---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:14.580505-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: #."
lastmod: '2024-03-13T22:44:52.129904-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0938\u0947 \u0924\u093E\
  \u0930\u0940\u0916 \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u093E"
weight: 30
---

## कैसे करें:


### `java.time` पैकेज का उपयोग करते हुए (जावा 8 और बाद में सिफारिशी):
```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "2023-04-30";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        LocalDate date = LocalDate.parse(dateString, formatter);
        System.out.println(date); // आउटपुट: 2023-04-30
    }
}
```

### `SimpleDateFormat` का उपयोग करते हुए (पुरानी विधि):
```java
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "30/04/2023";
        SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy");
        try {
            Date date = formatter.parse(dateString);
            System.out.println(date); // आउटपुट प्रारूप आपके सिस्टम के डिफ़ॉल्ट प्रारूप पर निर्भर करता है
        } catch (ParseException e) {
            e.printStackTrace();
        }
    }
}
```

### तृतीय-पक्ष पुस्तकालयों का उपयोग करते हुए (उदाहरण के लिए, Joda-Time):
Joda-Time एक महत्वपूर्ण तृतीय-पक्ष पुस्तकालय रहा है, लेकिन जावा 8 में `java.time` पैकेज के परिचय के बाद अब रख-रखाव मोड में है। हालांकि, 8 से पहले के जावा संस्करणों का उपयोग करने वालों के लिए, Joda-Time एक अच्छा विकल्प है।
```java
import org.joda.time.LocalDate;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "2023-04-30";
        DateTimeFormatter formatter = DateTimeFormat.forPattern("yyyy-MM-dd");
        LocalDate date = LocalDate.parse(dateString, formatter);
        System.out.println(date); // आउटपुट: 2023-04-30
    }
}
```
ध्यान दें कि जब दिनांकों के साथ काम करते हैं, तो हमेशा समय क्षेत्र की सेटिंग्स के प्रति सचेत रहें, यदि आप केवल दिनांकों के बजाय दिनांक-समयों को पार्स या प्रारूपित कर रहे हैं।

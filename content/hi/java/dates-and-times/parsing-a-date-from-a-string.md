---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:14.580505-07:00
description: "\u090F\u0915 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0938\
  \u0947 \u0926\u093F\u0928\u093E\u0902\u0915 \u092A\u093E\u0930\u094D\u0938 \u0915\
  \u0930\u0928\u093E \u092F\u0939 \u0938\u0941\u0928\u093F\u0936\u094D\u091A\u093F\
  \u0924 \u0915\u0930\u0924\u093E \u0939\u0948 \u0915\u093F \u0926\u093F\u0928\u093E\
  \u0902\u0915 \u0914\u0930 \u0938\u092E\u092F \u0915\u093E \u092A\u093E\u0920\u094D\
  \u092F \u092A\u094D\u0930\u0924\u093F\u0928\u093F\u0927\u093F\u0924\u094D\u0935\
  \ `Date` \u0911\u092C\u094D\u091C\u0947\u0915\u094D\u091F \u092F\u093E \u090F\u0915\
  \ \u0905\u0927\u093F\u0915 \u0906\u0927\u0941\u0928\u093F\u0915 `LocalDateTime`\
  \ \u0911\u092C\u094D\u091C\u0947\u0915\u094D\u091F\u2026"
lastmod: '2024-03-13T22:44:52.129904-06:00'
model: gpt-4-0125-preview
summary: "\u090F\u0915 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0938\u0947\
  \ \u0926\u093F\u0928\u093E\u0902\u0915 \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\
  \u0928\u093E \u092F\u0939 \u0938\u0941\u0928\u093F\u0936\u094D\u091A\u093F\u0924\
  \ \u0915\u0930\u0924\u093E \u0939\u0948 \u0915\u093F \u0926\u093F\u0928\u093E\u0902\
  \u0915 \u0914\u0930 \u0938\u092E\u092F \u0915\u093E \u092A\u093E\u0920\u094D\u092F\
  \ \u092A\u094D\u0930\u0924\u093F\u0928\u093F\u0927\u093F\u0924\u094D\u0935 `Date`\
  \ \u0911\u092C\u094D\u091C\u0947\u0915\u094D\u091F \u092F\u093E \u090F\u0915 \u0905\
  \u0927\u093F\u0915 \u0906\u0927\u0941\u0928\u093F\u0915 `LocalDateTime` \u0911\u092C\
  \u094D\u091C\u0947\u0915\u094D\u091F\u2026"
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0938\u0947 \u0924\u093E\
  \u0930\u0940\u0916 \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u093E"
weight: 30
---

## क्या और क्यों?
एक स्ट्रिंग से दिनांक पार्स करना यह सुनिश्चित करता है कि दिनांक और समय का पाठ्य प्रतिनिधित्व `Date` ऑब्जेक्ट या एक अधिक आधुनिक `LocalDateTime` ऑब्जेक्ट में परिवर्तित हो। प्रोग्रामर इसे दिनांकों को संशोधित करने, प्रारूपित करने, तुलना करने या एक मानकीकृत प्रारूप में संग्रहीत करने के लिए करते हैं, जो दिनांक गणना, मान्यता, या सुसंगत अंतरराष्ट्रीयता की आवश्यकता वाले अनुप्रयोगों के लिए महत्वपूर्ण है।

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

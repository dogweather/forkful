---
title:                "स्ट्रिंग से तारीख पार्स करना"
aliases:
- /hi/java/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:15:14.580505-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग से तारीख पार्स करना"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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

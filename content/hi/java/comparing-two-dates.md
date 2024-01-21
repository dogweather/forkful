---
title:                "दो तारीखों की तुलना"
date:                  2024-01-20T17:33:19.971220-07:00
model:                 gpt-4-1106-preview
simple_title:         "दो तारीखों की तुलना"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

तारीखों की तुलना का मतलब है दो डेट्स को आपस में मिलाना या उनका क्रम जानना। प्रोग्रामर इसे अक्सर समय-सीमित कार्यों, ईवेंट शेड्यूलिंग और डेटा वैलिडेशन में करते हैं।

## How to (कैसे करें):

Java में, `LocalDate`, `LocalDateTime`, और `ZonedDateTime` क्लासेस द्वारा आसानी से डेट्स की तुलना की जा सकती है। चलिए देखते हैं कैसे:

```java
import java.time.LocalDate;

public class DateComparison {
    public static void main(String[] args) {
        LocalDate date1 = LocalDate.of(2023, 4, 1);
        LocalDate date2 = LocalDate.now();

        if(date1.isBefore(date2)) {
            System.out.println("Date1 is before Date2");
        } else if(date1.isEqual(date2)) {
            System.out.println("Date1 is the same as Date2");
        } else {
            System.out.println("Date1 is after Date2");
        }
    }
}
```

इस कोड का आउटपुट आज की तारीख पर निर्भर करेगा।

## Deep Dive (गहन जानकारी):

तारीखों की तुलना का इतिहास Java के संस्करणों के साथ बदला है। `java.util.Date` और `java.util.Calendar` से शुरू होकर, Java 8 में `java.time` पैकेज ने इस काम को आसान और अधिक आधुनिक बना दिया।

पुराने `Date` और `Calendar` ऑब्जेक्ट्स के बजाय, `LocalDate` का इस्तेमाल करना बेहतर है क्योंकि यह immutable होते हैं और thread-safe होते हैं। इसके अलावा, `LocalDate` में केवल दिन, महीने और साल होते हैं, न कि टाइम ज़ोन और टाइम जैसे एक्स्ट्रा डिटेल्स।

यदि आपको समय और टाइम ज़ोन के साथ तारीख की तुलना करनी है, तो `ZonedDateTime` का प्रयोग करें।

```java
import java.time.ZonedDateTime;
import java.time.ZoneId;

public class DateTimeComparison {
    public static void main(String[] args) {
        ZonedDateTime dateTime1 = ZonedDateTime.now(ZoneId.of("Asia/Kolkata"));
        ZonedDateTime dateTime2 = ZonedDateTime.now(ZoneId.of("America/New_York"));

        if(dateTime1.isBefore(dateTime2)) {
            System.out.println("DateTime1 is before DateTime2");
        } else if(dateTime1.isEqual(dateTime2)) {
            System.out.println("DateTime1 is the same as DateTime2");
        } else {
            System.out.println("DateTime1 is after DateTime2");
        }
    }
}
```

यहां भी आउटपुट वर्तमान समय पर निर्भर करेगा।

## See Also (और भी देखें):

- [Oracle's Java Tutorials - Date Time](https://docs.oracle.com/javase/tutorial/datetime/)
- [Baeldung's Guide on Java 8 Date Time API](https://www.baeldung.com/java-8-date-time-intro)
- [JavaDoc for LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
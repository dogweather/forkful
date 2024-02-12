---
title:                "भविष्य या अतीत में तारीख की गणना"
aliases:
- /hi/java/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:30.302047-07:00
model:                 gpt-4-1106-preview
simple_title:         "भविष्य या अतीत में तारीख की गणना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

तारीख का हिसाब करना मतलब, भविष्य या अतीत में किसी दिन का पता लगाना है। प्रोग्रामर्स इसका उपयोग ईवेंट्स की तारीखें सेट करने, डेडलाइंस की गणना करने और टाइम-सेंसिटिव डेटा प्रोसेसिंग के लिए करते हैं। 

## कैसे करें:

```java
import java.time.LocalDate;
import java.time.Period;

public class DateCalculation {
    public static void main(String[] args) {
        LocalDate today = LocalDate.now();

        // 10 दिन आगे की तारीख
        LocalDate tenDaysLater = today.plusDays(10);
        System.out.println("10 दिन बाद की तारीख: " + tenDaysLater);

        // 2 महीने और 3 दिन पीछे की तारीख
        LocalDate beforeTwoMonthsThreeDays = today.minusPeriod(Period.ofMonths(2)).minusDays(3);
        System.out.println("2 महीने और 3 दिन पहले की तारीख: " + beforeTwoMonthsThreeDays);
    }
}
```

सैंपल आउटपुट:

```
10 दिन बाद की तारीख: 2023-04-20
2 महीने और 3 दिन पहले की तारीख: 2023-01-25
```

## गहराई से समझें:

तारीख की गणना करना, कैलेंडर की खोज के समय से महत्वपूर्ण रहा है। Java में, `java.util.Date` और `java.util.Calendar` जैसे पुराने क्लासेस का प्रयोग होता था, लेकिन वे असुविधाजनक और गलतियों से भरे हुए थे। Java 8 के साथ आने वाले `java.time` package ने काम आसान और सही बना दिया। `LocalDate`, `LocalTime`, और `LocalDateTime` वगैरह समय और तिथि संबंधी कार्यों के लिए आधुनिक सोल्यूशंस प्रदान करते हैं। 

भविष्य या अतीत की तारीख की गणना करते समय, लीप इयर्स और टाइम जोन्स जैसी चीजें भी महत्वपूर्ण होती हैं, और `java.time` इन्हें ध्यान में रखता है।

वैकल्पिक तरीकों में, थर्ड-पार्टी लाइब्रेरीज जैसे कि Joda-Time पहले काफी प्रचलित थी, लेकिन Java 8 से `java.time` का प्रयोग ज्यादा सामान्य है।

## और जानकारी के लिए:

- [LocalDate JavaDoc](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [java.time package overview](https://docs.oracle.com/javase/tutorial/datetime/overview/)
- [Oracle's Date Time tutorials](https://docs.oracle.com/javase/tutorial/datetime/index.html)

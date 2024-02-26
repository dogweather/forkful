---
date: 2024-01-20 17:31:30.302047-07:00
description: "\u0924\u093E\u0930\u0940\u0916 \u0915\u093E \u0939\u093F\u0938\u093E\
  \u092C \u0915\u0930\u0928\u093E \u092E\u0924\u0932\u092C, \u092D\u0935\u093F\u0937\
  \u094D\u092F \u092F\u093E \u0905\u0924\u0940\u0924 \u092E\u0947\u0902 \u0915\u093F\
  \u0938\u0940 \u0926\u093F\u0928 \u0915\u093E \u092A\u0924\u093E \u0932\u0917\u093E\
  \u0928\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\
  \u092E\u0930\u094D\u0938 \u0907\u0938\u0915\u093E \u0909\u092A\u092F\u094B\u0917\
  \ \u0908\u0935\u0947\u0902\u091F\u094D\u0938 \u0915\u0940 \u0924\u093E\u0930\u0940\
  \u0916\u0947\u0902 \u0938\u0947\u091F \u0915\u0930\u0928\u0947, \u0921\u0947\u0921\
  \u0932\u093E\u0907\u0902\u0938 \u0915\u0940 \u0917\u0923\u0928\u093E \u0915\u0930\
  \u0928\u0947 \u0914\u0930\u2026"
lastmod: '2024-02-25T18:49:49.338639-07:00'
model: gpt-4-1106-preview
summary: "\u0924\u093E\u0930\u0940\u0916 \u0915\u093E \u0939\u093F\u0938\u093E\u092C\
  \ \u0915\u0930\u0928\u093E \u092E\u0924\u0932\u092C, \u092D\u0935\u093F\u0937\u094D\
  \u092F \u092F\u093E \u0905\u0924\u0940\u0924 \u092E\u0947\u0902 \u0915\u093F\u0938\
  \u0940 \u0926\u093F\u0928 \u0915\u093E \u092A\u0924\u093E \u0932\u0917\u093E\u0928\
  \u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\
  \u0930\u094D\u0938 \u0907\u0938\u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0908\
  \u0935\u0947\u0902\u091F\u094D\u0938 \u0915\u0940 \u0924\u093E\u0930\u0940\u0916\
  \u0947\u0902 \u0938\u0947\u091F \u0915\u0930\u0928\u0947, \u0921\u0947\u0921\u0932\
  \u093E\u0907\u0902\u0938 \u0915\u0940 \u0917\u0923\u0928\u093E \u0915\u0930\u0928\
  \u0947 \u0914\u0930\u2026"
title: "\u092D\u0935\u093F\u0937\u094D\u092F \u092F\u093E \u0905\u0924\u0940\u0924\
  \ \u092E\u0947\u0902 \u0924\u093E\u0930\u0940\u0916 \u0915\u0940 \u0917\u0923\u0928\
  \u093E"
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

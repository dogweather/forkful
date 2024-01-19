---
title:                "भविष्य या अतीत में तारीख का हिसाब"
html_title:           "Kotlin: भविष्य या अतीत में तारीख का हिसाब"
simple_title:         "भविष्य या अतीत में तारीख का हिसाब"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

भविष्य या भूतकाल में किसी तारीख की गणना सॉफ़्टवेयर का एक सामान्य कार्य है। प्रोग्रामर इसे समय-संबंधी फ़ंक्शनालिटी बनाने, तारीखों की प्राधिक्य की पद्धतियाँ बनाने, या खुद की कास्टम कैलेंडर एप्लीकेशन बनाने के लिए करते हैं। 

## कैसे करें:

भविष्य में किसी तारीख की गणना करने के लिए, Kotlin का LocalData.now और plusDays विधि उपयोगी है।

```Kotlin
import java.time.LocalDate

fun main(args: Array<String>) {
    val today = LocalDate.now()
    println("आज की तारीख: $today")

    val futureDate = today.plusDays(5)
    println("5 दिनों बाद की तारीख: $futureDate")
}
```
आउटपुट:

```
आज की तारीख: 2022-10-12
5 दिनों बाद की तारीख: 2022-10-17
```

## गहरी डाइव:

Kotlin में तारीखों की गणना जावा 8+ के `java.time` पैकेज और उसकी LocalDate, LocalTime, LocalDateTime क्लासेज पर आधारित होती है। इन्हें JSR-310 कुछ नई तारीख और समय API के तौर पर भी जाना जाता है। कुछ अन्य विकल्प Joda-Time और Date4j भी हैं, लेकिन `java.time` का उपयोग करना अच्छा है क्योंकि यह JDK से आता है और व्यापक रूप से प्रमाणित है। 

## और देखें:

1. Kotlin और Java तारीख और समय API: https://kotlinlang.org/api/latest/jvm/stdlib/java.time/
2. JSR-310 (जावा तारीख-समय API): https://www.baeldung.com/java-8-date-time-intro
3. Joda-Time: https://www.joda.org/joda-time/
4. Date4j: http://www.date4j.net/
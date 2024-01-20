---
title:                "दो तारीखों की तुलना"
html_title:           "Elixir: दो तारीखों की तुलना"
simple_title:         "दो तारीखों की तुलना"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

दो तारीखों की तुलना करने का अर्थ है, हम देखते हैं कि कौन सी तारीख पहली है और कौन सी बाद में है। प्रोग्रामर्स इसे इवेंट या किसी नियमित कार्य को संभालने, समय की मदद से डाटा को व्यवस्थित करने, या समय आवट की गणना करने के लिए करते हैं। 

## कैसे करें:

जावा में दो तारीखों की तुलना के लिए हम `java.time.LocalDate` का उपयोग कर सकते हैं। 

```Java
import java.time.LocalDate;

public class CompareDates {
    public static void main(String[] args) {
        LocalDate date1 = LocalDate.of(2022, 1, 1);
        LocalDate date2 = LocalDate.of(2023, 1, 1);

        if (date1.isBefore(date2)) {
            System.out.println("date1 पहले है");
        } else {
            System.out.println("date2 पहले है");
        }
    }
}
```

ऊपरी कोड का आउटपुट होगा:

`date1 पहले है`

## गहराई में:

1. पुराने Java versions में, कलेंडर और डेट classes का इस्तेमाल तारीखों की तुलना के लिए किया जाता था, लेकिन वे समय क्षेत्र और समर्थन issues के कारण समस्याएं पैदा करते थे। 

2. कुछ अन्य विकल्प `java.time.Instant`, `java.time.ZonedDateTime`, और Joda-Time library हैं। यह उपयोगी होता है जब हमें टाइमज़ोन के साथ तारीखों की तुलना करनी हो। 

3. `java.time.LocalDate` वर्ग का `isBefore()`, `isAfter()` और `isEqual()` method हमें दो तारीखों की तुलना करने में मदद करते हैं।

## और भी देखें:

1. Java 8 Date/Time API (java.time pakage): https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html 
2. Joda-Time library: https://www.joda.org/joda-time/
3. java.util.Date vs java.util.Calendar: https://www.baeldung.com/java-date-vs-calendar
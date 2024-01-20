---
title:                "भविष्य या अतीत में एक तारीख की गणना"
html_title:           "Java: भविष्य या अतीत में एक तारीख की गणना"
simple_title:         "भविष्य या अतीत में एक तारीख की गणना"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

भविष्य या अतीत में एक तारीख की गणना का मतलब है कि हम किसी विशेष तारीख को, वर्तमान तारीख से अग्रसर या पिछले, खोज रहे हैं। प्रोग्रामर इसे तकनीकी आवश्यकताओं को पूरा करने के लिए करते हैं, जैसे कि उद्घाटन समय की गणना या पासवर्ड की कालवादी की जांच।

## कैसे करें: 

यहाँ जावा के लिए एक साधारण कोड स्निपेट दिया गया है जिसे आप किसी भविष्य की तारीख की गणना के लिए उपयोग कर सकते हैं।

```Java
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;

public class Main {
  public static void main(String[] args) {
    LocalDate today = LocalDate.now();
    LocalDate futureDate = today.plus(5, ChronoUnit.DAYS);
    System.out.println("Today's date: " + today);
    System.out.println("Date after 5 days: " + futureDate);
  }
}
```
इस कोड का आउटपुट होगा:
```
Today's date: 2021-09-02
Date after 5 days: 2021-09-07
```
## गहरा डाइव:

जावा में LocalDate और ChronoUnit के क्लास का उपयोग करके कल्पनात्मक तारीख की गणना की जाती है, यह सबसे आम और सही तरीका है। हालांकि, ऐसे अन्य तरीके भी मौजूद हैं, जैसे कि Calendar क्लास और Date क्लास, लेकिन ये पुराने हैं और अप्रचलित (deprecated) का कारण बनते हैं।

## देखने के लिए भी:

- आपको अधिक विवरण के लिए [Java's Date and Time API](https://docs.oracle.com/javase/tutorial/datetime/) मार्गदर्शिका पढ़नी चाहिए।
- [Baeldung's guide to Java Dates](https://www.baeldung.com/java-8-date-time-intro) यह पन्ना भी उपयोगी हो सकता है यदि आपको जावा में तारीख और समय के साथ काम करने में मदद की जरुरत है।
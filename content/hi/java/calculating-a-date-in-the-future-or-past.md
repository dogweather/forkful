---
title:                "Java: भविष्य या भूत की तिथि की गणना"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्यों

दिनांक की गणना आने या बीते हुए कुछ अतीत या भविष्य में होने वाली घटनाओं के संबंध में उपयोगी हो सकती है।

## कैसे करें

```Java
import java.time.LocalDate;

// एक साल बाद की तारीख की गणना करें
LocalDate today = LocalDate.now();
LocalDate nextYear = today.plusYears(1);
System.out.println("आज से एक साल बाद की तारीख: " + nextYear);

// पिछले साल की तारीख की गणना करें
LocalDate lastYear = today.minusYears(1);
System.out.println("आज से पिछले साल की तारीख: " + lastYear);

// भविष्य के वालेंटाइन्स डे की तारीख की गणना करें
LocalDate valentinesDay = LocalDate.of(2022, 2, 14);
System.out.println("2022 में वालेन्टाइन्स डे की तारीख: " + valentinesDay);
```

आउटपुट:

```
आज से एक साल बाद की तारीख: 2022-10-10
आज से पिछले साल की तारीख: 2020-10-10
2022 में वालेन्टाइन्स डे की तारीख: 2022-02-14
```

## गहराई में जाएं

Java में तारीख की गणना करने के लिए `LocalDate` क्लास का उपयोग किया जाता है। इसके साथ हम अक्सर अतीत या भविष्य में एक विशिष्टता या ईवेंट की तारीख गणना करना चाहते हैं। Java में, `LocalDate` क्लास के साथ हम आसानी से इस गणना को कर सकते हैं। इसके अलावा, हम अन्य तारीख संबंधित कार्यों को भी कर सकते हैं जैसे कि तारीखों को मिलाना, तोड़ना, तारीखों को अनुमति देना आदि। इस तरह हमारे पास तारीखों को गणना और प्रबंधित करने के लिए शक्तिशाली एवं सरल टूल्स हैं।

## इससे जुड़े देखें

[Java LocalDate दस्तावेज़ीकरण](https://docs.oracle.com
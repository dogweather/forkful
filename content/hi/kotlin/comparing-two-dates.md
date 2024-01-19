---
title:                "दो तारीखों की तुलना"
html_title:           "Elixir: दो तारीखों की तुलना"
simple_title:         "दो तारीखों की तुलना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्या औऱ क्यों?

दो तारीखों की तुलना करना मतलब होता है समझना कि कौन सी तारीख पहली है और कौन सी दूसरी। प्रोग्रामर्स यह करते हैं ताकि उन्हें निर्धारित करने में मदद मिले कि कौन सी घटना पहले हुई थी और कौन सी बाद में।

## कैसे करें:

```Kotlin
import java.time.LocalDate

fun main() {
    val date1 = LocalDate.of(2020, 1, 1)
    val date2 = LocalDate.of(2021, 1, 1)

    when {
        date1.isBefore(date2) -> println("तारीख 1 पहले है।")
        date1.isAfter(date2) -> println("तारीख 2 पहले है।")
        else -> println("दोनों तारीखें समान हैं।")
    }
}
```

उत्पादन: "तारीख 1 पहले है।"

## गहराई में

**ऐतिहासिक संदर्भ:** जब जावा 8 ने LocalDate श्रेणी का परिचय दिया, Kotlin में तारीखों की तुलना करने का सबसे आम और सरल तरीका बन गया।

**विकल्प:** हालांकि, यदि आपके पास LocalDate के इस्तेमाल का विकल्प नहीं है, तो आप compareTo फ़ंक्शन का इस्तेमाल कर सकते हैं, जो कि किसी अन्य तारीख के सापेक्ष एक तारीख की पूर्वता, उत्तरता या समानता पर आधारित एक इंटेजर यानी संख्या देता है।

**विवरण:** LocalDate वर्ग के गुण `isBefore` और `isAfter` तारीखों की तुलना करने के लिए उपयोग होते हैं। `isBefore` तब true (सच) देता है जब वर्तमान तारीख पेसेड तारीख से पहली होती है, और `isAfter` दूसरी तारीख से बाद में होने पर true देता है। 

## देखने के लिए 

[Kotlin official Documentation](https://kotlinlang.org/docs/home.html)

[Java 8 LocalDate API Docs](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)

[Good resource for learning Kotlin](https://developer.android.com/kotlin/learn)
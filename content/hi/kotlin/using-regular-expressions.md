---
title:                "रेगुलर एक्सप्रेशन का उपयोग"
date:                  2024-01-19
html_title:           "Bash: रेगुलर एक्सप्रेशन का उपयोग"
simple_title:         "रेगुलर एक्सप्रेशन का उपयोग"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
रेगुलर एक्सप्रेशन (Regular Expressions) एक पावरफुल पैटर्न मैचिंग टूल है जो टेक्स्ट से खास जानकारी निकालने, वैलिडेशन और सर्च-रिप्लेस कामों के लिए इस्तेमाल होता है। प्रोग्रामर्स इसे इफेक्टिव टेक्स्ट प्रोसेसिंग, डेटा एक्सट्रेक्शन और डेटा मैनिपुलेशन के लिए यूज करते हैं।

## How to: (कैसे करें:)
```Kotlin
fun main() {
    val regex = Regex("[a-zA-Z]+")
    val input = "Regex ki Jai Ho 123"

    // पैटर्न मैचिंग
    val matches = regex.findAll(input)
    matches.forEach { matchResult ->
        println(matchResult.value)
    }

    // स्ट्रिंग रेप्लेसमेंट
    val replaced = input.replace(regex, "Kotlin")
    println(replaced)
}

// आउटपुट:
// Regex
// ki
// Jai
// Ho
// Kotlin Kotlin Kotlin Kotlin 123
```

## Deep Dive (गहराई में जानकारी)
रेगुलर एक्सप्रेशन, 1950 से 1970 के बीच कंप्यूटर साइंस में विकसित किए गए थे। Perl और UNIX टूल्स ने इसे लोकप्रिय बनाया। Kotlin में रेगेक्स का इस्तेमाल `Regex` क्लास के जरिए होता है, जो जावा की `Pattern` और `Matcher` क्लासेस पर आधारित है। अल्टरनेटिव्स में स्ट्रिंग फंक्शन्स और पूरी तरह से पार्सिंग अल्गोरिदम्स हो सकते हैं। पर, रेगेक्स तेज़ और कॉम्पैक्ट सोल्यूशन्स प्रोवाइड करता है।

## See Also (और देखें)
- Kotlin Regular Expression Documentation: [Kotlin Regex](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- Regex101: रेगुलर एक्सप्रेशन को आज़माने के लिए एक ऑनलाइन टूल [Regex101](https://regex101.com/)
- Regexr: एक और इंटरैक्टिव टूल जो एक्सप्लेनेशन्स के साथ आता है [Regexr](https://regexr.com/)

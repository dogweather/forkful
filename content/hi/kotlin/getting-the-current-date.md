---
title:                "Kotlin: वर्तमान तारीख प्राप्त करना"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्यों
कोटलिन में मौजूदा तारीख प्राप्त करने में क्या लाभ है, इसके बारे में है।

## कैसे करें
कोटलिन में संपूर्ण तारीख प्राप्त करने के लिए, आपको `java.time.LocalDate` का उपयोग करना होगा। निम्न उदाहरण में, हम `LocalDate.now()` का उपयोग करके वर्तमान तारीख प्राप्त करेंगे और प्रिंट करेंगे।

```Kotlin
val localDate = LocalDate.now()
println("वर्तमान तारीख: $localDate")
```
आउटपुट:
```
वर्तमान तारीख: 2021-09-23
```

आप इस तरीके से `java.time.format.DateTimeFormatter` का भी उपयोग करके तारीख को अपने अनुभव के अनुसार प्रिंट कर सकते हैं। निम्न उदाहरण में, हम `DateTimeFormatter.ofPattern("dd MMMM yyyy", Locale("hi"))` का उपयोग करके अंग्रेजी से हिंदी में तारीख को प्रिंट करते हुए इसका उदाहरण देख सकते हैं। 

```Kotlin
val localDate = LocalDate.now()
val dateFormat = DateTimeFormatter.ofPattern("dd MMMM yyyy", Locale("hi"))
val formattedDate = localDate.format(dateFormat)
println("वर्तमान तारीख: $formattedDate")
```
आउटपुट:
```
वर्तमान तारीख: २३ सितंबर २०२१
```

## गहराई में जाएं
`java.time.LocalDate` कोटलिन में विशिष्ट तारीख को संजोने और प्रस्तुत करने के लिए उपयोगी एक श्रेणी है। आप `java.time.format.DateTimeFormatter` के साथ आगे बढ़कर तारीखों को अधिक संवेदनशील ढंग से प्रस्तुत कर सकते हैं। इसके अलावा, आप अपने एप्लिकेशन में तारीखों के साथ विभिन्न गणनाएं कर सकते हैं और उनको अपने उदाहरणों में शामिल कर सकते हैं। 

## देखें भी
- [Kotlin documentation for LocalDate](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time.-local-date/)
- [Java documentation for LocalDate](https://docs.oracle.com
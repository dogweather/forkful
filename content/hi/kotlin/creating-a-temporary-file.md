---
title:                "एक अस्थायी फ़ाइल बनाना"
html_title:           "Kotlin: एक अस्थायी फ़ाइल बनाना"
simple_title:         "एक अस्थायी फ़ाइल बनाना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्यों 

व्यक्ति अस्थायी फाइलों को बनाने में शामिल होना क्योंकि यह एक सुरक्षित विकल्प हो सकता है।

## कैसे करें 

कोट्लिन में अस्थानिक फ़ाइल बनाने के 3 आसान तरीके हैं: 

```Kotlin
// तरीका १: इस्तेमाल करें 'tempFile' फ़ंक्शन
val tempFile = createTempFile()
println(tempFile.name)
```

```Kotlin
// तरीका २: यूनिक नाम के साथ फ़ाइल बनाएं
val prefix = "temp"
val suffix = ".txt"
val tempFile = File.createTempFile(prefix, suffix)
println(tempFile.name)
```

```Kotlin
// तरीका ३: फ़ाइल पथ और नाम तय करें
val directory = File("tempFiles")
val tempFile = File.createTempFile("temp", null, directory)
println(tempFile.name)
```

**आउटपुट:** 
```
example8260740204719179421.tmp
temp5023458318972597807.txt
temp7649214401290860755.kt
```

## गहराई में जाएं 

अस्थायी फ़ाइलें बनाने के अलावा, कोट्लिन में आप उन्हें अन्य विशिष्ट फ़ंक्शनों के साथ भी उपयोग कर सकते हैं। उदाहरण के लिए, आप इन अस्थायी फ़ाइलों को डायरेक्टरी में स्थानांतरित कर सकते हैं या उन्हें साफ़ कर सकते हैं। इसके अलावा, कोट्लिन में अस्थायी फ़ाइलों को डेटा या स्ट्रीम से पढ़ा और लिखा जा सकता है। 

## देखें भी 

- [कोट्लिन डॉक्यूमेंटेशन](https://kotlinlang.org/docs/reference/)
- [अस्थायी फ़ाइलों के लिए डॉक्यूमेंटेशन](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/create-temp-file.html)
- [कोट्लिन ट्यूटोरियल](https://kotlinlang.org/docs/tutorials/)
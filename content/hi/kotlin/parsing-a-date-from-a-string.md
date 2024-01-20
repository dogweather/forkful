---
title:                "एक स्ट्रिंग से तारीख पार्स करना"
html_title:           "C++: एक स्ट्रिंग से तारीख पार्स करना"
simple_title:         "एक स्ट्रिंग से तारीख पार्स करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

"स्ट्रिंग से तारीख पार्स करना" एक काम है जिसमें हम किसी तारीख को स्ट्रिंग रूप में प्रदान करते हैं और प्रोग्राम इसे वास्तविक तारीख में बदल देता है। प्रोग्रामर्स इसे करते हैं ताकि वे इंटरफेस, डेटाबेस, और अन्य जगहों से मिलने वाले स्ट्रिंग डेटा को काम में ले सकें। 

## कैसे करें:

```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

val dateFormatter = DateTimeFormatter.ofPattern("dd-MM-yyyy")
val dateAsString = "23-12-2021"
val actualDate = LocalDate.parse(dateAsString, dateFormatter)

println(actualDate)  // Output: 2021-12-23
```

इस कोड का उद्देश्य स्ट्रिंग "23-12-2021" को एक `LocalDate` ऑब्जेक्ट में परिवर्तित करना है। `DateTimeFormatter` का उपयोग दिनांक प्रारूप को सेट करने के लिए किया जाता है, जो कि पार्सर को स्थानीय दिनांक में बदलने के लिए दिशानिर्देश देता है। 

## गहरा विवरण

1. **ऐतिहासिक प्रसंग**: "स्ट्रिंग को तारीख में पार्स करना" की आवश्यकता तब होती जब इंटरनेट पर डेटा आदान-प्रदान पहले हुआ था। डेटा का आदान-प्रदान स्ट्रिंग रूप में ही संभव था क्योंकि इसे नेटवर्क के माध्यम से भेजा जा सकता है। 

2. **विकल्प**: विभिन्न भाषाओं में तारीख पार्स करने के विभिन्न तरीके हो सकते हैं। जावा में `SimpleDateFormat` का उपयोग किया जा सकता है, PHP में `date_create_from_format` तथा Python में `datetime.strptime` का उपयोग किया जाता है। 

3. **कार्यान्वयन का विवरण**: अधिकतर मामलों में, दिनांक पार्स करने की क्रियाएँ जटिल नहीं होती हैं। यद्यपि, ध्यान देने योग्य बात यह है कि अलग अलग संस्कृतियों में तारीखें अलग अलग प्रारूपों में हो सकती हैं, जो डेटा पार्स करने के प्रक्रिया को जटिल बना सकती है। 

## यदि आप और अधिक जानना चाहते हैं:

- [Java 8 Date-Time API]("https://www.journaldev.com/2800/java-8-date-localdate-localdatetime-instant") -  Java 8 में नए डेट-टाइम API के बारे में अधिक जानकारी के लिए। 
- [DateTimeFormatter]("https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html") - DateTimeFormatter वर्ग का विवरण और उपयोग की जानकारी। 
- [ISO Date-Time Standards](https://www.w3.org/TR/NOTE-datetime) - ISO के द्वारा मान्यता प्राप्त तारीख-समय प्रारूप के बारे में अधिक।
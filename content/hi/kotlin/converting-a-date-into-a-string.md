---
title:                "तारीख को स्ट्रिंग में रूपांतरण करना"
html_title:           "Kotlin: तारीख को स्ट्रिंग में रूपांतरण करना"
simple_title:         "तारीख को स्ट्रिंग में रूपांतरण करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्यों

डेटा को स्ट्रिंग में रूपांतरित करने का काम आसान हो सकता है, लेकिन अगर आपको कोडिंग में थोड़ी सी भी भूली हो तो यह एक अवांछित उत्तराधिकार हो सकता है। जब हम समय (date) को स्ट्रिंग में रूपांतरित करते हैं, तो उसका प्रयोग डेटा को स्टोर करने, प्रिंट करने या इस्तेमाल करने के लिए किया जाता है।

## कैसे करे

```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

// वर्तमान तारीख को लिखने के लिए LocalDate ऑब्जेक्ट बनाएं
val currentDate = LocalDate.now()

// टेम्पलेट में डेटा प्रिंट करने के लिए format बनाएं
val dateFormat = DateTimeFormatter.ofPattern("dd/MMM/yyyy")

// स्ट्रिंग में रूपांतरित करने के लिए format का प्रयोग करें
val formattedDate = currentDate.format(dateFormat)

// प्रिंट करें
println("आज की तारीख है: $formattedDate")

// अन्य फॉर्मेट का उपयोग करके स्ट्रिंग फॉर्मेटिंग को बदलें
val newFormat = DateTimeFormatter.ofPattern("dd-MM-yyyy")
val newFormattedDate = currentDate.format(newFormat)

// नए फॉर्मेट में प्रिंट करें
println("आज की तारीख है: $newFormattedDate")
```

### आउटपुट:

```Kotlin
आज की तारीख है: 06/अप्रैल/2021
आज की तारीख है: 06-04-2021
```

## गहराई में

डेटा को स्ट्रिंग में रूपांतरित करने की प्रक्रिया `java.time` लाइब्रेरी का उपयोग करके की जाती है जो जावा 8 और बाद के संस्करणों में उपलब्ध है। और यदि आपके पास स्टार्ट अप के लिए एक प्रोजेक्ट हो, तो `SimpleDateFormat` का उपयोग करके भी डेटा को स्ट्रिंग में रूपांतरित कर सकते हैं।

## देखने के लिए

- [Kotlin डेटा टाइम मेथॉड](https://k
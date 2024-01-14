---
title:    "Kotlin: तारीख को स्ट्रिंग में रूपांतरण करना"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## क्यों

आजकल समय के साथ-साथ हर काम को ऑनलाइन लेकर जाना जाता है, और इसमें तिथि की स्ट्रिंग में बदलने की जरूरत पड़ती है। यह ब्लॉग पोस्ट हिंदी रीडर्स के लिए कोटलिन प्रोग्रामिंग में तारीख को स्ट्रिंग में बदलने से संबंधित है।

## कैसे करें

```Kotlin
val date = Date()
val sdf = SimpleDateFormat("dd-MM-yyyy")
val dateString = sdf.format(date)
println(dateString)
```

इस कोड से आप तारीख को स्ट्रिंग में बदल सकते हैं। यहाँ हमने `SimpleDateFormat` का उपयोग किया है जो की तारीख को चाहे जिस फॉर्मेट में बदल सकता है। और आप अपनी इच्छानुसार फॉर्मेट भी चयन कर सकते हैं। यहाँ हमने dd-MM-yyyy चयन किया है जिससे तारीख को दिन, माह और साल के अनुसार स्ट्रिंग में बदला गया है। 

## गहराई में जाएं

यदि आप अपनी कोडिंग को अधिक प्रोफेशनल और शास्त्रीय बनाना चाहते हैं, तो आप इस विषय में गहराई में जाना चाहेंगे। तारीख को स्ट्रिंग में बदलने के लिए `SimpleDateFormat` के अलग-अलग मेथड्स भी हैं जैसे `format`, `parse`, `getTime` आदि। इन सब के अलावा आप दिन, माह, साल, सप्ताह का नाम आदि भी स्ट्रिंग में बदल सकते हैं। 

## देखें भी

- [Kotlin Strings and String Interpolation](https://www.javatpoint.com/kotlin-strings)
- [Working with Dates and Times in Kotlin](https://www.bezkoder.com/kotlin-date-time/)
---
title:                "स्ट्रिंग से उद्धरण चिह्न हटाना"
aliases:
- /hi/kotlin/removing-quotes-from-a-string/
date:                  2024-01-26T03:42:19.978378-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग से उद्धरण चिह्न हटाना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

एक स्ट्रिंग से उद्धरण चिह्न हटाना मतलब हर एक उद्धरण चिह्न को, चाहे वह एकल (') हो या दोहरा (" "), जिस टेक्स्ट डेटा पर आप काम कर रहे हैं, उससे निकाल देना है। प्रोग्रामर्स को अक्सर डेटा साफ-सफाई के लिए, आगे की प्रोसेसिंग के लिए तैयार करने या जब उद्धरण चिह्न खुद डेटा के अर्थ से संबंधित नहीं होते, तब इसे करने की जरूरत पड़ती है।

## कैसे करें:

यहाँ Kotlin में एक स्ट्रिंग से दोनों प्रकार के उद्धरण चिह्न हटाने का एक साधारण तरीका है:

```kotlin
fun removeQuotes(input: String): String {
    return input.replace("\"", "").replace("'", "")
}

fun main() {
    val stringWithQuotes = "Kotlin \"rocks\" it's 'cool'"
    val stringWithoutQuotes = removeQuotes(stringWithQuotes)
    println(stringWithoutQuotes) // परिणाम: Kotlin rocks its cool
}
```

और यदि आप केवल एक प्रकार के उद्धरण चिह्न को हटाना चाहते हैं, तो दूसरे replace कॉल को छोड़ दें।

```kotlin
fun removeDoubleQuotes(input: String): String {
    return input.replace("\"", "")
}

fun removeSingleQuotes(input: String): String {
    return input.replace("'", "")
}

fun main() {
    val stringWithQuotes = "Kotlin \"rocks\" it's 'cool'"
    println(removeDoubleQuotes(stringWithQuotes)) // परिणाम: Kotlin rocks it's 'cool'
    println(removeSingleQuotes(stringWithQuotes)) // परिणाम: Kotlin "rocks" its cool
}
```

## गहराई में:

ऐतिहासिक रूप से, स्ट्रिंग्स और कैरेक्टर्स को एस्केप करना प्रोग्रामिंग का एक मुख्य हिस्सा रहा है, क्योंकि टेक्स्ट वह मूलभूत तरीका है जिससे हम डेटा से इंटरैक्ट करते हैं। स्ट्रिंग्स में कभी-कभी उद्धरण चिह्नों को एस्केप करने की जरूरत होती है। इसे एक पूर्ववर्ती बैकस्लैश द्वारा दिखाया जाता है (जैसे, `"She said, \"Hi!\""`). ऐसे स्ट्रिंग्स को प्रोसेस करते समय, आपको एस्केप कैरेक्टर्स, या खुद उद्धरण चिह्नों को हटाने की ज़रूरत हो सकती है, ताकि टेक्स्ट को साफ या अधिक उपयोगी बनाया जा सके।

`replace` विधि के विकल्पों में रेगेक्स-आधारित हटान या मैन्युअली स्ट्रिंग को कैरेक्टर दर कैरेक्टर पार्स करना शामिल है। हालांकि, रेगेक्स सरल ऑपरेशंस के लिए ओवरकिल हो सकता है और मैन्युअल पार्सिंग बिल्ट-इन स्ट्रिंग फंक्शंस का उपयोग करने से कम कुशल होती है। Kotlin का `replace` फंक्शन नीचे के Java के `String` `replace` मेथड का लाभ उठाता है, जो प्रदर्शन के लिए अच्छी तरह से अनुकूलित है।

लागू करने के संदर्भ में, यह उल्लेख करना लायक है कि Kotlin Java के साथ इंटरऑपरेबल है, इसलिए, वास्तव में, आप जो भी ऑपरेशंस स्ट्रिंग्स पर करते हैं, वे Java में होने जितने प्रदर्शनकारी होंगे। उद्धरण चिह्नों को हटाते समय किनारे के मामलों, जैसे कि नेस्टेड उद्धरण, के बारे में जागरूक रहना महत्वपूर्ण होता है, जिनके लिए एक अधिक सोफिस्टिकेटेड दृष्टिकोण की आवश्यकता हो सकती है, संभवतः नियमित अभिव्यक्तियों या एक पार्सर लाइब्रेरी का उपयोग करके।

## और देखें

Kotlin में स्ट्रिंग्स को हैंडल करने पर अधिक संदर्भ के लिए, आधिकारिक दस्तावेज़ीकरण देख सकते हैं:

- [Kotlin का स्ट्रिंग दस्तावेज़ीकरण](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)

Kotlin में नियमित अभिव्यक्तियों और पार्सिंग में गहराई से उतरने के लिए:

- [Kotlin Regex दस्तावेज़ीकरण](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)

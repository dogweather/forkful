---
title:                "JSON के साथ काम करना"
date:                  2024-01-19
html_title:           "Arduino: JSON के साथ काम करना"
simple_title:         "JSON के साथ काम करना"

category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
JSON (JavaScript Object Notation) एक लाइटवेट डेटा-इंटरचेंज फॉर्मेट है। प्रोग्रामर्स इसे वेब APIs के जरिए डेटा शेयर और स्टोर करने के लिए करते हैं क्योंकि यह बहुत ही आसान, पढ़ने योग्य, और मशीन के जरिए प्रोसेस करने में भी सहज है।

## How to: (कैसे करें:)
Kotlin में JSON को हैंडल करना बहुत ही सीधा है। `kotlinx.serialization` एक आम लाइब्रेरी है इसके लिए। पहले आप इसे अपने `build.gradle` में डिपेंडेंसी के रूप में जोड़ें:

```gradle
dependencies {
    implementation "org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.2"
}
```

फिर JSON से काम करने के लिए:

```kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

@Serializable
data class User(val name: String, val age: Int)

fun main() {
    val jsonString = """
    {
        "name": "Amit",
        "age": 30
    }
    """.trimIndent()

    // JSON स्ट्रिंग को कोटलिन ऑब्जेक्ट में पार्स करना
    val user = Json.decodeFromString<User>(jsonString)
    println(user) // User(name=Amit, age=30)

    // कोटलिन ऑब्जेक्ट को JSON स्ट्रिंग में कन्वर्ट करना
    val jsonOutput = Json.encodeToString(user)
    println(jsonOutput) // {"name":"Amit","age":30}
}
```

## Deep Dive (गहराई से जानकारी)
JSON फॉर्मेट 2000 के दशक की शुरुआत में पॉपुलर हुआ था। यह XML का एक विकल्प है और अक्सर इसे अधिक संक्षिप्त और तेज़ माना जाता है। Kotlin में `kotlinx.serialization` के अलावा, और भी लाइब्रेरीज हैं जैसे कि Gson और Moshi जो JSON सीरियलाइजेशन/डिसीरियलाइजेशन प्रदान करते हैं। हालांकि, `kotlinx.serialization` Kotlin की ऑफिशिअल लाइब्रेरी है और नेटिव कोरुटिन सपोर्ट के साथ बेहतरीन इंटीग्रेशन प्रदान करती है।

## See Also (और भी जानकारी के लिए)
- Kotlin Serialization डॉक्यूमेंटेशन: [https://kotlinlang.org/docs/serialization.html](https://kotlinlang.org/docs/serialization.html)
- JSON क्या है और कैसे काम करता है: [https://www.json.org/json-en.html](https://www.json.org/json-en.html)
- kotlinx.serialization GitHub: [https://github.com/Kotlin/kotlinx.serialization](https://github.com/Kotlin/kotlinx.serialization)
- Gson GitHub: [https://github.com/google/gson](https://github.com/google/gson)
- Moshi GitHub: [https://github.com/square/moshi](https://github.com/square/moshi)

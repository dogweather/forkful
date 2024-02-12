---
title:                "JSON के साथ काम करना"
aliases:
- /hi/kotlin/working-with-json.md
date:                  2024-02-03T19:24:46.755969-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSON के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
Kotlin में JSON (JavaScript Object Notation) के साथ काम करना JSON डेटा को पार्स करने और जेनरेट करने को शामिल करता है। प्रोग्रामर इसे आसानी से विभिन्न परतों में एक एप्लिकेशन के बीच डेटा का आदान-प्रदान करने या वेब सेवाओं के साथ संवाद करने के लिए करते हैं, JSON के हल्के और मानव-पठनीय प्रारूप के कारण।

## कैसे करें:
Kotlin में JSON के लिए बिल्ट-इन समर्थन शामिल नहीं है लेकिन यह `Gson` जो कि Google द्वारा और `Kotlinx.serialization` जो कि JetBrains द्वारा तीसरे पक्ष के पुस्तकालयों की शक्तिशाली सुविधाओं का लाभ उठाता है। यहाँ आप दोनों का उपयोग करके JSON के साथ काम कैसे कर सकते हैं।

### Gson का उपयोग करना
अपनी `build.gradle` फ़ाइल में Gson निर्भरता जोड़ें:
```kotlin
implementation 'com.google.code.gson:gson:2.8.9'
```

JSON स्ट्रिंग को एक ऑब्जेक्ट में पार्स करना और इसके विपरीत:
```kotlin
import com.google.gson.Gson

// एक डेटा क्लास डिफाइन करें
data class User(val name: String, val age: Int)

fun main() {
    val gson = Gson()

    // Serialize
    val json = gson.toJson(User("John Doe", 30))
    println(json)  // आउटपुट: {"name":"John Doe","age":30}

    // Deserialize
    val user: User = gson.fromJson(json, User::class.java)
    println(user)  // आउटपुट: User(name=John Doe, age=30)
}
```

### Kotlinx.serialization का उपयोग करना
पहले, अपनी `build.gradle` में निर्भरता शामिल करें:
```kotlin
implementation "org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.3"
```

इसके बाद, अपने बिल्ड स्क्रिप्ट के शीर्ष पर `kotlinx-serialization` प्लगइन लागू करें:
```kotlin
plugins {
    kotlin("jvm") version "1.6.10"
    kotlin("plugin.serialization") version "1.6.10"
}
```

Kotlinx.serialization के साथ सेरियलाइज़िंग और डिसेरियलाइज़िंग:
```kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

// एक सीरियलाइज़ करने योग्य डेटा क्लास डिफाइन करें
@Serializable
data class User(val name: String, val age: Int)

fun main() {
    // Serialize
    val json = Json.encodeToString(User("Jane Doe", 28))
    println(json)  // आउटपुट: {"name":"Jane Doe","age":28}

    // Deserialize
    इसके बाद वह उपयोगकर्ता को Json.decodeFromString<User>(json) के माध्यम से उतारता है
    println(user)  // आउटपुट: User(name=Jane Doe, age=28)
}
```

Gson और Kotlinx.serialization दोनों ही Kotlin एप्लिकेशनों में JSON के साथ काम करना सरल बनाते हैं, एक को दूसरे पर प्राथमिकता देना आपकी विशिष्ट परियोजना आवश्यकताओं और व्यक्तिगत पसंद पर निर्भर करता है।

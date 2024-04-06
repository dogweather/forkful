---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:46.755969-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Kotlin \u092E\u0947\
  \u0902 JSON \u0915\u0947 \u0932\u093F\u090F \u092C\u093F\u0932\u094D\u091F-\u0907\
  \u0928 \u0938\u092E\u0930\u094D\u0925\u0928 \u0936\u093E\u092E\u093F\u0932 \u0928\
  \u0939\u0940\u0902 \u0939\u0948 \u0932\u0947\u0915\u093F\u0928 \u092F\u0939 `Gson`\
  \ \u091C\u094B \u0915\u093F Google \u0926\u094D\u0935\u093E\u0930\u093E \u0914\u0930\
  \ `Kotlinx.serialization` \u091C\u094B \u0915\u093F JetBrains \u0926\u094D\u0935\
  \u093E\u0930\u093E\u2026"
lastmod: '2024-04-05T21:53:54.299362-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u092E\u0947\u0902 JSON \u0915\u0947 \u0932\u093F\u090F \u092C\u093F\
  \u0932\u094D\u091F-\u0907\u0928 \u0938\u092E\u0930\u094D\u0925\u0928 \u0936\u093E\
  \u092E\u093F\u0932 \u0928\u0939\u0940\u0902 \u0939\u0948 \u0932\u0947\u0915\u093F\
  \u0928 \u092F\u0939 `Gson` \u091C\u094B \u0915\u093F Google \u0926\u094D\u0935\u093E\
  \u0930\u093E \u0914\u0930 `Kotlinx.serialization` \u091C\u094B \u0915\u093F JetBrains\
  \ \u0926\u094D\u0935\u093E\u0930\u093E \u0924\u0940\u0938\u0930\u0947 \u092A\u0915\
  \u094D\u0937 \u0915\u0947 \u092A\u0941\u0938\u094D\u0924\u0915\u093E\u0932\u092F\
  \u094B\u0902 \u0915\u0940 \u0936\u0915\u094D\u0924\u093F\u0936\u093E\u0932\u0940\
  \ \u0938\u0941\u0935\u093F\u0927\u093E\u0913\u0902 \u0915\u093E \u0932\u093E\u092D\
  \ \u0909\u0920\u093E\u0924\u093E \u0939\u0948\u0964 \u092F\u0939\u093E\u0901 \u0906\
  \u092A \u0926\u094B\u0928\u094B\u0902 \u0915\u093E \u0909\u092A\u092F\u094B\u0917\
  \ \u0915\u0930\u0915\u0947 JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E\
  \ \u0915\u0948\u0938\u0947 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\
  \u0964."
title: "JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
weight: 38
---

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

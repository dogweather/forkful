---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:46.755969-07:00
description: "Kotlin \u092E\u0947\u0902 JSON (JavaScript Object Notation) \u0915\u0947\
  \ \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E JSON \u0921\u0947\
  \u091F\u093E \u0915\u094B \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u0947\
  \ \u0914\u0930 \u091C\u0947\u0928\u0930\u0947\u091F \u0915\u0930\u0928\u0947 \u0915\
  \u094B \u0936\u093E\u092E\u093F\u0932 \u0915\u0930\u0924\u093E \u0939\u0948\u0964\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\u0947\
  \ \u0906\u0938\u093E\u0928\u0940 \u0938\u0947 \u0935\u093F\u092D\u093F\u0928\u094D\
  \u0928\u2026"
lastmod: '2024-03-13T22:44:52.295731-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u092E\u0947\u0902 JSON (JavaScript Object Notation) \u0915\u0947\
  \ \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E JSON \u0921\u0947\
  \u091F\u093E \u0915\u094B \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u0947\
  \ \u0914\u0930 \u091C\u0947\u0928\u0930\u0947\u091F \u0915\u0930\u0928\u0947 \u0915\
  \u094B \u0936\u093E\u092E\u093F\u0932 \u0915\u0930\u0924\u093E \u0939\u0948\u0964\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\u0947\
  \ \u0906\u0938\u093E\u0928\u0940 \u0938\u0947 \u0935\u093F\u092D\u093F\u0928\u094D\
  \u0928 \u092A\u0930\u0924\u094B\u0902 \u092E\u0947\u0902 \u090F\u0915 \u090F\u092A\
  \u094D\u0932\u093F\u0915\u0947\u0936\u0928 \u0915\u0947 \u092C\u0940\u091A \u0921\
  \u0947\u091F\u093E \u0915\u093E \u0906\u0926\u093E\u0928-\u092A\u094D\u0930\u0926\
  \u093E\u0928 \u0915\u0930\u0928\u0947 \u092F\u093E \u0935\u0947\u092C \u0938\u0947\
  \u0935\u093E\u0913\u0902 \u0915\u0947 \u0938\u093E\u0925 \u0938\u0902\u0935\u093E\
  \u0926 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0915\u0930\u0924\
  \u0947 \u0939\u0948\u0902, JSON \u0915\u0947 \u0939\u0932\u094D\u0915\u0947 \u0914\
  \u0930 \u092E\u093E\u0928\u0935-\u092A\u0920\u0928\u0940\u092F \u092A\u094D\u0930\
  \u093E\u0930\u0942\u092A \u0915\u0947 \u0915\u093E\u0930\u0923\u0964."
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

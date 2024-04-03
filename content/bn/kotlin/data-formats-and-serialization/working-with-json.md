---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:29:34.551435-06:00
description: "Kotlin-\u098F JSON (JavaScript Object Notation) \u098F\u09B0 \u09B8\u09BE\
  \u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 JSON\
  \ \u09A1\u09BE\u099F\u09BE \u09AA\u09BE\u09B0\u09CD\u09B8 \u098F\u09AC\u0982 \u099C\
  \u09C7\u09A8\u09BE\u09B0\u09C7\u099F \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF\
  \ \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u09A8 \u098F\u0995\u099F\u09BF \u0985\
  \u09CD\u09AF\u09BE\u09AA\u09CD\u09B2\u09BF\u0995\u09C7\u09B6\u09A8\u09C7\u09B0 \u09AC\
  \u09BF\u09AD\u09BF\u09A8\u09CD\u09A8\u2026"
lastmod: '2024-03-17T18:47:44.016061-06:00'
model: gpt-4-0125-preview
summary: "Kotlin-\u098F JSON (JavaScript Object Notation) \u098F\u09B0 \u09B8\u09BE\
  \u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 JSON\
  \ \u09A1\u09BE\u099F\u09BE \u09AA\u09BE\u09B0\u09CD\u09B8 \u098F\u09AC\u0982 \u099C\
  \u09C7\u09A8\u09BE\u09B0\u09C7\u099F \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF\
  \ \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u09A8 \u098F\u0995\u099F\u09BF \u0985\
  \u09CD\u09AF\u09BE\u09AA\u09CD\u09B2\u09BF\u0995\u09C7\u09B6\u09A8\u09C7\u09B0 \u09AC\
  \u09BF\u09AD\u09BF\u09A8\u09CD\u09A8 \u09B8\u09CD\u09A4\u09B0\u09C7\u09B0 \u09AE\
  \u09A7\u09CD\u09AF\u09C7 \u09AC\u09BE \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09BE\
  \u09B0\u09CD\u09AD\u09BF\u09B8 \u09B8\u09B9 \u09A1\u09BE\u099F\u09BE \u0986\u09A6\
  \u09BE\u09A8-\u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u09B8\u09B9\u099C \u0995\u09B0\
  \u09BE\u09B0 \u099C\u09A8\u09CD\u09AF, JSON \u098F\u09B0 \u09B9\u09BE\u09B2\u0995\
  \u09BE \u0993 \u09AE\u09BE\u09A8\u09AC-\u09AA\u09BE\u09A0\u09AF\u09CB\u0997\u09CD\
  \u09AF \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F\u09C7\u09B0 \u0995\u09BE\u09B0\
  \u09A3\u09C7\u0964."
title: "JSON \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE"
weight: 38
---

## কি এবং কেন?
Kotlin-এ JSON (JavaScript Object Notation) এর সাথে কাজ করা মানে JSON ডাটা পার্স এবং জেনারেট করা। প্রোগ্রামাররা এটি করে থাকেন একটি অ্যাপ্লিকেশনের বিভিন্ন স্তরের মধ্যে বা ওয়েব সার্ভিস সহ ডাটা আদান-প্রদান সহজ করার জন্য, JSON এর হালকা ও মানব-পাঠযোগ্য ফরম্যাটের কারণে।

## কিভাবে:
Kotlin স্বয়ংক্রিয় ভাবে JSON-এর জন্য কোনো সাপোর্ট অন্তর্ভুক্ত করে না কিন্তু `Gson` (গুগল দ্বারা) এবং `Kotlinx.serialization` (JetBrains দ্বারা) এর মতো তৃতীয় পক্ষের লাইব্রেরির শক্তিশালী বৈশিষ্ট্যগুলো ব্যবহার করে। এখানে আপনি JSON এর সাথে কাজ করার জন্য উভয়কে কিভাবে ব্যবহার করতে পারেন তা দেখানো হলো।

### Gson ব্যবহার করে
আপনার `build.gradle` ফাইলে Gson নির্ভরতা যোগ করুন:
```kotlin
implementation 'com.google.code.gson:gson:2.8.9'
```

JSON স্ট্রিং থেকে অবজেক্টে এবং তার বিপরীতে পার্স করা:
```kotlin
import com.google.gson.Gson

// ডেটা ক্লাস নির্ধারণ
data class User(val name: String, val age: Int)

fun main() {
    val gson = Gson()

    // Serialize
    val json = gson.toJson(User("John Doe", 30))
    println(json)  // আউটপুট: {"name":"John Doe","age":30}

    // Deserialize
    val user: User = gson.fromJson(json, User::class.java)
    println(user)  // আউটপুট: User(name=John Doe, age=30)
}
```

### Kotlinx.serialization ব্যবহার করে
প্রথমে, আপনার `build.gradle`-এ নির্ভরতা অন্তর্ভুক্ত করুন:
```kotlin
implementation "org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.3"
```

এরপর, আপনার বিল্ড স্ক্রিপ্টের শীর্ষে `kotlinx-serialization` প্লাগিন প্রয়োগ করুন:
```kotlin
plugins {
    kotlin("jvm") version "1.6.10"
    kotlin("plugin.serialization") version "1.6.10"
}
```

Kotlinx.serialization ব্যবহারে সিরিয়ালাইজ করা এবং ডি-সিরিয়ালাইজ করা:
```kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

// একটি সিরিয়ালাইজযোগ্য ডেটা ক্লাস নির্ধারণ
@Serializable
data class User(val name: String, val age: Int)

fun main() {
    // Serialize
    val json = Json.encodeToString(User("Jane Doe", 28))
    println(json)  // আউটপুট: {"name":"Jane Doe","age":28}

    // Deserialize
    val user = Json.decodeFromString<User>(json)
    println(user)  // আউটপুট: User(name=Jane Doe, age=28)
}
```

Gson এবং Kotlinx.serialization উভয়েই Kotlin অ্যাপ্লিকেশনে JSON এর সাথে কাজ করা সহজ করে, একটি অন্যটির উপরে পছন্দ করা আপনার নির্দিষ্ট প্রকল্প প্রয়োজনীয়তা ও ব্যক্তিগত পছন্দের উপর নির্ভর করে।

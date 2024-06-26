---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:06:20.753185-06:00
description: "\u0995\u09C0\u09AD\u09BE\u09AC\u09C7: Kotlin \u099C\u09BE\u09AD\u09BE\
  \ \u09EE-\u098F \u099A\u09BE\u09B2\u09C1 \u09B9\u0993\u09AF\u09BC\u09BE `java.time`\
  \ \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\
  \u09AF\u09AE\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u09B0\u09CD\u09B8\
  \u09BF\u0982 \u09B8\u09BE\u09AA\u09CB\u09B0\u09CD\u099F \u0995\u09B0\u09C7\u0964\
  \ \u098F\u0996\u09BE\u09A8\u09C7 `LocalDateTime` \u098F\u09AC\u0982 \u098F\u0995\
  \u099F\u09BF \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09AA\u09CD\
  \u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09C7\u2026"
lastmod: '2024-03-17T18:47:44.003767-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u099C\u09BE\u09AD\u09BE \u09EE-\u098F \u099A\u09BE\u09B2\u09C1 \u09B9\
  \u0993\u09AF\u09BC\u09BE `java.time` \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C\u09C7\
  \u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982 \u09B8\u09BE\u09AA\u09CB\u09B0\u09CD\
  \u099F \u0995\u09B0\u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7 `LocalDateTime` \u098F\
  \u09AC\u0982 \u098F\u0995\u099F\u09BF \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\
  \u09CD\u099F \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8 \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u098F\u0995\u099F\u09BF \u09B8\
  \u09B9\u099C \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE \u0989\
  \u09AA\u09B8\u09CD\u09A5\u09BE\u09AA\u09BF\u09A4 \u09B9\u09B2."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A4\
  \u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 30
---

## কীভাবে:
Kotlin জাভা ৮-এ চালু হওয়া `java.time` প্যাকেজের মাধ্যমে তারিখ পার্সিং সাপোর্ট করে। এখানে `LocalDateTime` এবং একটি নির্দিষ্ট প্যাটার্ন ব্যবহার করে একটি সহজ প্রক্রিয়া উপস্থাপিত হল:

```kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun parseDateFromString(dateString: String): LocalDateTime {
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    return LocalDateTime.parse(dateString, formatter)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val date = parseDateFromString(dateString)
    println(date)  // আউটপুট: 2023-04-01T12:00
}
```

আরও নমনীয়তা পেতে বা এপিআইগুলির মতো বাহ্যিক সোর্স থেকে তারিখ হ্যান্ডেল করতে, আপনি Joda-Time এর মতো তৃতীয় পক্ষের লাইব্রেরি ব্যবহার করতে পারেন (যদিও `java.time` দৃঢ় থাকায় এটি এখন কম সাধারণ)। তবে, অধিকাংশ Kotlin অ্যাপ্লিকেশনের জন্য জেডিকে দ্বারা প্রদত্ত আধুনিক পদ্ধতি অনুসরণ করা পছন্দনীয়।

জাভা ৮ এর আগের সংস্করণগুলির জন্য বা এন্ড্রয়েড API লেভেলগুলিতে `java.time` সমর্থন না থাকার কারণে তৃতীয় পক্ষের লাইব্রেরি ব্যবহার না করে Kotlin-এ তারিখ পার্স করতে, আপনি এছাড়াও `SimpleDateFormat` ক্লাসের ব্যবহার করতে পারেন:

```kotlin
import java.text.SimpleDateFormat

fun parseDateUsingSimpleDateFormat(dateString: String): java.util.Date {
    val formatter = SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    return formatter.parse(dateString)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val date = parseDateUsingSimpleDateFormat(dateString)
    println(date)  // আউটপুট আপনার টাইমজোনের উপর নির্ভর করবে, উদাঃ, Sat Apr 01 12:00:00 GMT 2023
}
```

`SimpleDateFormat` ব্যবহার করার সময় সবসময় টাইমজোন সেট করতে ভুলবেন না, কারণ পার্স করা তারিখে অনাকাঙ্ক্ষিত অফসেট এড়াতে।

---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:33.924359-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Kotlin \u098F\u09B0 \u09B8\u09CD\
  \u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u09B2\u09BE\u0987\
  \u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7\
  \ \u09B0\u200D\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09AE \u09B8\u0982\u0996\u09CD\
  \u09AF\u09BE \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE\u09B0 \u098F\u0995\u099F\
  \u09BF \u09B8\u09B0\u09B2 \u0989\u09AA\u09BE\u09AF\u09BC \u09A6\u09C7\u0993\u09AF\
  \u09BC\u09BE \u09B9\u09AF\u09BC\u09C7\u099B\u09C7\u0964 \u098F\u0996\u09BE\u09A8\
  \u09C7 \u09AC\u09BF\u09AD\u09BF\u09A8\u09CD\u09A8 \u09A7\u09B0\u09A8\u09C7\u09B0\
  \ \u09B0\u200D\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09AE \u09AE\u09BE\u09A8 \u0989\
  \u09A4\u09CD\u09AA\u09BE\u09A6\u09A8 \u0995\u09B0\u09BE\u09B0\u2026"
lastmod: '2024-04-05T21:53:52.306498-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u098F\u09B0 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\
  \u09BE\u09B0\u09CD\u09A1 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u09B0\
  \ \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09B0\u200D\u09CD\u09AF\u09BE\u09A8\
  \u09CD\u09A1\u09AE \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09A4\u09C8\u09B0\u09BF\
  \ \u0995\u09B0\u09BE\u09B0 \u098F\u0995\u099F\u09BF \u09B8\u09B0\u09B2 \u0989\u09AA\
  \u09BE\u09AF\u09BC \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09AF\u09BC\u09C7\
  \u099B\u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u09AC\u09BF\u09AD\u09BF\u09A8\
  \u09CD\u09A8 \u09A7\u09B0\u09A8\u09C7\u09B0 \u09B0\u200D\u09CD\u09AF\u09BE\u09A8\
  \u09CD\u09A1\u09AE \u09AE\u09BE\u09A8 \u0989\u09A4\u09CD\u09AA\u09BE\u09A6\u09A8\
  \ \u0995\u09B0\u09BE\u09B0 \u0989\u09AA\u09BE\u09AF\u09BC \u09A6\u09C7\u0993\u09AF\
  \u09BC\u09BE \u09B9\u09B2\u0983."
title: "\u098F\u09B2\u09CB\u09AE\u09C7\u09B2\u09CB \u09B8\u0982\u0996\u09CD\u09AF\u09BE\
  \ \u0989\u09CE\u09AA\u09A8\u09CD\u09A8 \u0995\u09B0\u09BE"
weight: 12
---

## কিভাবে:
Kotlin এর স্ট্যান্ডার্ড লাইব্রেরির মাধ্যমে র‍্যান্ডম সংখ্যা তৈরি করার একটি সরল উপায় দেওয়া হয়েছে। এখানে বিভিন্ন ধরনের র‍্যান্ডম মান উত্পাদন করার উপায় দেওয়া হলঃ

### একটি র‍্যান্ডম ইন্টিজার তৈরি
নির্দিষ্ট সীমার মধ্যে একটি র‍্যান্ডম ইন্টিজার তৈরি করতে:

```kotlin
import kotlin.random.Random

fun main() {
    val randomNumber = Random.nextInt(1, 100) // 1 এবং 99 এর মধ্যে একটি র‍্যান্ডম সংখ্যা তৈরি করে
    println(randomNumber)
}
```

### একটি র‍্যান্ডম ডাবল তৈরি
তেমনিভাবে, একটি র‍্যান্ডম ডাবল তৈরি করতে:

```kotlin
import kotlin.random.Random

fun main() {
    val randomDouble = Random.nextDouble(1.0, 10.0) // 1.0 এবং 10.0 এর মধ্যে একটি র‍্যান্ডম ডাবল তৈরি করে
    println(randomDouble)
}
```

### একটি র‍্যান্ডম বুলিয়ান তৈরি
একটি র‍্যান্ডম বুলিয়ান মান তৈরি করতে:

```kotlin
import kotlin.random.Random

fun main() {
    val randomBoolean = Random.nextBoolean() // যথাক্রমে সত্য বা মিথ্যা র‍্যান্ডমভাবে তৈরি করে
    println(randomBoolean)
}
```

### পুনরুৎপাদনযোগ্য ফলাফলের জন্য সীডিং
র‍্যান্ডম সংখ্যার পুনরুৎপাদনযোগ্য ধারাবাহিকতা প্রয়োজন হলে (উদাহরণস্বরূপ, পরীক্ষায়), আপনি র‍্যান্ডম সংখ্যা জেনারেটরে সীডিং করতে পারেন:

```kotlin
import kotlin.random.Random

fun main() {
    val seed = 12345L
    val random = Random(seed)
    val randomNumber = random.nextInt(1, 100)
    println(randomNumber)
}
```

## গভীর ডুব
Kotlin স্ট্যান্ডার্ড লাইব্রেরির র‍্যান্ডম সংখ্যা তৈরির পদ্ধতি প্রকৃতপক্ষে `java.util.Random` কে অধীনে নিয়ে এসেছে, যা ব্যবহারে সুবিধা এবং কর্মক্ষমতার একটি মিশ্রণ নিশ্চিত করে। তবে, এটি গুরুত্বপূর্ণ মনে রাখা দরকার যে এই পদ্ধতিগুলি প্যুডো-র‍্যান্ডম সংখ্যা উত্পাদন করে,যা মানে হচ্ছে সংখ্যাগুলি র‍্যান্ডম মনে হলেও এগুলি একটি নির্ধারক প্রক্রিয়া ব্যবহার করে তৈরি করা হয়।

অধিকাংশ প্রয়োগের জন্য Kotlin এর `Random` শ্রেণীর প্রদত্ত র‍্যান্ডমনেস যথেষ্ট। তবে, নিরাপত্তা-সংবেদনশীল প্রয়োগ, যেমন ক্রিপ্টোগ্রাফি, যেখানে র‍্যান্ডমনেসের মান পরমার্থ, সেখানে আপনার `java.security.SecureRandom` ব্যবহার বিবেচনা করা উচিৎ। SecureRandom বিশেষভাবে ক্রিপ্টোগ্রাফিক অপারেশনের জন্য ডিজাইন করা, যা উচ্চমানের র‍্যান্ডমনেস প্রদান করে, যদিও এতে সম্ভাব্য কর্মক্ষমতা বাণিজ্য থাকতে পারে।

Kotlin চাকা পুনঃআবিষ্কার করে না, কিন্তু Java-র র‍্যান্ডম সংখ্যা উত্পাদন প্রক্রিয়ার উপর একটি Kotlin-বান্ধব API প্রস্তাব করে, যা Kotlin প্রকল্পগুলিতে আরও স্বজ্ঞানী এবং সাবলীল ব্যবহার করা যায়। যেমন সবসময়, র‍্যান্ডমনেস নিয়ে কাজ করার সময়, প্রোগ্রামারদের কাজের উপযুক্ত সরঞ্জাম নির্বাচন করতে সাবধানে বিবেচনা করা উচিৎ।

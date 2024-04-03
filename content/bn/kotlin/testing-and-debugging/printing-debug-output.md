---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:07:29.344537-06:00
description: "\u0995\u09C0\u09AD\u09BE\u09AC\u09C7: \u099A\u09B2\u09C1\u09A8 \u0995\
  \u09A8\u09B8\u09CB\u09B2\u09C7 \u0995\u09BF\u099B\u09C1 \u09AA\u09CD\u09B0\u09BF\
  \u09A8\u09CD\u099F \u0995\u09B0\u09BF."
lastmod: '2024-03-17T18:47:43.996040-06:00'
model: gpt-4-0125-preview
summary: "\u099A\u09B2\u09C1\u09A8 \u0995\u09A8\u09B8\u09CB\u09B2\u09C7 \u0995\u09BF\
  \u099B\u09C1 \u09AA\u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09BF."
title: "\u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AA\
  \u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09BE"
weight: 33
---

## কীভাবে:
চলুন কনসোলে কিছু প্রিন্ট করি:

```Kotlin
fun main() {
    val magicNumber = 42
    println("জাদুর নম্বর হলো $magicNumber")

    debugPrint("জাদুর নম্বর বর্গকৃত হলে সমান ${magicNumber * magicNumber}")
}

fun debugPrint(message: String) {
    if (BuildConfig.DEBUG) {
        println("ডিবাগ: $message")
    }
}
```
নমুনা আউটপুট:
```
জাদুর নম্বর হলো 42
ডিবাগ: জাদুর নম্বর বর্গকৃত হলে সমান 1764
```
দ্রুত এবং নোংরা, আপনি দেখতে পাচ্ছেন আপনার মানগুলি কনসোলে ঠিক সেখানে।

## গভীর ডাইভ
ডিবাগিং উদ্দেশ্যে কনসোলে প্রিন্ট করা পুরানো কালের প্রথা। এটি সহজ, সকল প্রোগ্রামিং ভাষায় প্রচলিত, এবং কাজ সম্পন্ন করে। তবে, এটি বিলাসবহুল নয়, এবং জটিল সিস্টেমে, অনেক আউটপুট গোলমাল হতে পারে।

কোটলিনে `println` এর বিকল্প হিসেবে `Log4j` বা কোটলিনের নিজস্ব `Logging` ইউটিলিটি ব্যবহার করা যেতে পারে, যা গুরুত্ব মাত্রা অনুসারে বার্তা ফিল্টার করার সহায়তা করে।

কোটলিনে একটি নতুনত্ব, যেমন আমাদের `debugPrint` ফাংশনে দেখা যায়, তা হল আমরা যদি একটি ডিবাগ বিল্ডে থাকি তবে চেক করুন; এর মাধ্যমে, আমরা আমাদের ডিবাগ বার্তাগুলো দিয়ে প্রোডাকশন লগগুলি জঞ্জালমুক্ত রাখি, এবং আমাদের প্রকৃত বিতরণগুলি পরিষ্কার ও ব্যবহারকারী-বান্ধব রাখি।

## আরও দেখুন
- কোটলিনে লগিং এর প্রবর্তন জানতে, অফিসিয়াল ডক্সে যান: [Kotlin Logging](https://github.com/MicroUtils/kotlin-logging)
- JetBrains’ ডিবাগিং কৌশল সম্পর্কে তাদের মতামত: [IntelliJ IDEA Debugging](https://www.jetbrains.com/help/idea/debugging-code.html)
- যদি আপনি অ্যান্ড্রয়েড ব্যবহার করেন, তাহলে Logcat ব্যবহার করে অফিসিয়াল গাইড অমূল্য: [Android Logcat Documentation](https://developer.android.com/studio/command-line/logcat)

---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:49.423604-06:00
description: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u0995\
  \u09CD\u09B8\u099F\u09CD\u09B0\u09CD\u09AF\u09BE\u0995\u09CD\u099F \u0995\u09B0\u09BE\
  \ \u09AE\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\
  \u09CD\u099F \u0985\u0982\u09B6 \u099F\u09C7\u09A8\u09C7 \u09AC\u09C7\u09B0 \u0995\
  \u09B0\u09BE\u0964 \u0986\u09AE\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09BF\
  \ \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09A1\u09BE\u099F\u09BE \u09A8\u09BF\u09AF\
  \u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE\u09B0 \u0985\u09A5\u09AC\u09BE\
  \ \u09AC\u09BF\u09B6\u09CD\u09B2\u09C7\u09B7\u09A3 \u0995\u09B0\u09BE\u09B0 \u099C\
  \u09A8\u09CD\u09AF, \u09AF\u09C7\u09AE\u09A8\u2026"
lastmod: '2024-03-17T18:47:43.982136-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u0995\
  \u09CD\u09B8\u099F\u09CD\u09B0\u09CD\u09AF\u09BE\u0995\u09CD\u099F \u0995\u09B0\u09BE\
  \ \u09AE\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\
  \u09CD\u099F \u0985\u0982\u09B6 \u099F\u09C7\u09A8\u09C7 \u09AC\u09C7\u09B0 \u0995\
  \u09B0\u09BE\u0964 \u0986\u09AE\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09BF\
  \ \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09A1\u09BE\u099F\u09BE \u09A8\u09BF\u09AF\
  \u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE\u09B0 \u0985\u09A5\u09AC\u09BE\
  \ \u09AC\u09BF\u09B6\u09CD\u09B2\u09C7\u09B7\u09A3 \u0995\u09B0\u09BE\u09B0 \u099C\
  \u09A8\u09CD\u09AF, \u09AF\u09C7\u09AE\u09A8\u2026"
title: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AC\u09C7\u09B0\
  \ \u0995\u09B0\u09BE"
weight: 6
---

## কি এবং কেন?
সাবস্ট্রিং এক্সট্র্যাক্ট করা মানে একটি স্ট্রিং থেকে নির্দিষ্ট অংশ টেনে বের করা। আমরা এটি করি টেক্সট ডাটা নিয়ে কাজ করার অথবা বিশ্লেষণ করার জন্য, যেমন ইমেইল ঠিকানা থেকে ব্যবহারকারীর নাম আহরণ করা বা তারিখ থেকে মাস পেতে স্লাইস করা।

## কিভাবে:
কোটলিনে, `substring`, `take`, এবং `drop` ফাংশনগুলি ব্যবহার করুন।

```Kotlin
fun main() {
    val text = "Hello, Kotlin!"

    println(text.substring(7, 13)) // প্রিন্ট করে "Kotlin"
    
    // শুরু থেকে
    println(text.take(5)) // প্রিন্ট করে "Hello"

    // শেষ থেকে
    println(text.takeLast(6)) // প্রিন্ট করে "Kotlin!"

    // ক্যারেক্টার ড্রপ করা
    println(text.drop(7)) // প্রিন্ট করে "Kotlin!"
}
```

## গভীরে দেখা
প্রোগ্রামিং এর প্রাথমিক দিনে, স্ট্রিং সম্ভাল করা ম্যানুয়াল এবং ভুল প্রবণ ছিল। কোটলিনে, এটি আরও সহজ, নিরাপদ এবং কম সম্পদ ব্যবহারকারী, বিল্ট-ইন ফাংশন এবং স্ট্রিং ক্লাসের ফিচার্সের ধন্যবাদে।

`substring` এর বিকল্পগুলো অন্তর্ভুক্ত করে `Regex` ব্যবহার করে নিয়মিত এক্সপ্রেশন বা স্ট্রিংসকে কাটাকাটি করার জন্য `split` - কিন্তু এই পদ্ধতিগুলি সহজ কাজের জন্য অতিরিক্ত হতে পারে।

ইমপ্লিমেন্টেশনের দিক থেকে, মনে রাখবেন যে কোটলিনে স্ট্রিংগুলি ইমিউটেবল। সুতরাং, যখন আপনি একটি সাবস্ট্রিং এক্সট্র্যাক্ট করেন, আপনি আসলে একটি নতুন স্ট্রিং অবজেক্ট তৈরি করছেন, মূলটিকে পরিবর্তন করছেন না।

## দেখুন এছাড়াও
- কোটলিন স্ট্রিং ডকুমেন্টেশন: [কোটলিন স্ট্রিং](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- উন্নত স্ট্রিং ম্যানিপুলেশনের জন্য কোটলিনের রেগেক্স: [কোটলিন রেগেক্স](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)

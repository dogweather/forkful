---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:23.501767-06:00
description: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09B8\u0982\u09AF\u09CB\u099C\
  \u09A8 \u09A0\u09BF\u0995 \u09B8\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u0989\u0987\
  \u099A \u09A4\u09C8\u09B0\u09BF\u09B0 \u09AE\u09A4\u0987, \u09A4\u09AC\u09C7 \u09AC\
  \u09CD\u09B0\u09C7\u09A1 \u098F\u09AC\u0982 \u09AA\u09C1\u09B0\u09A3\u09C7\u09B0\
  \ \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09C7, \u0986\u09AA\u09A8\u09BF \u09B6\
  \u09AC\u09CD\u09A6\u0997\u09C1\u09B2\u09CB\u0995\u09C7 \u098F\u0995\u09B8\u09BE\u09A5\
  \u09C7 \u099C\u09A1\u09BC\u09BF\u09AF\u09BC\u09C7 \u098F\u0995\u099F\u09BF \u09AC\
  \u09BE\u0995\u09CD\u09AF \u09AC\u09BE \u09AB\u09CD\u09B0\u09C7\u099C \u09A4\u09C8\
  \u09B0\u09BF \u0995\u09B0\u099B\u09C7\u09A8\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\u2026"
lastmod: '2024-03-17T18:47:43.985304-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09B8\u0982\u09AF\u09CB\u099C\
  \u09A8 \u09A0\u09BF\u0995 \u09B8\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u0989\u0987\
  \u099A \u09A4\u09C8\u09B0\u09BF\u09B0 \u09AE\u09A4\u0987, \u09A4\u09AC\u09C7 \u09AC\
  \u09CD\u09B0\u09C7\u09A1 \u098F\u09AC\u0982 \u09AA\u09C1\u09B0\u09A3\u09C7\u09B0\
  \ \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09C7, \u0986\u09AA\u09A8\u09BF \u09B6\
  \u09AC\u09CD\u09A6\u0997\u09C1\u09B2\u09CB\u0995\u09C7 \u098F\u0995\u09B8\u09BE\u09A5\
  \u09C7 \u099C\u09A1\u09BC\u09BF\u09AF\u09BC\u09C7 \u098F\u0995\u099F\u09BF \u09AC\
  \u09BE\u0995\u09CD\u09AF \u09AC\u09BE \u09AB\u09CD\u09B0\u09C7\u099C \u09A4\u09C8\
  \u09B0\u09BF \u0995\u09B0\u099B\u09C7\u09A8\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u099C\u09CB\u09A1\u09BC\u09BE\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE"
weight: 3
---

## কি ও কেন?

স্ট্রিং সংযোজন ঠিক স্যান্ডউইচ তৈরির মতই, তবে ব্রেড এবং পুরণের পরিবর্তে, আপনি শব্দগুলোকে একসাথে জড়িয়ে একটি বাক্য বা ফ্রেজ তৈরি করছেন। প্রোগ্রামাররা ডায়নামিক টেক্সট তৈরি করতে, যেমন একজন ব্যবহারকারীর নাম সহ একটি অভিবাদন দেখানো, অথবা অন-দি-ফ্লাই ফাইল পাথ তৈরি করতে স্ট্রিং সংযোজন করে।

## কিভাবে:

এখানে দেখানো হলো কিভাবে Kotlinএ স্ট্রিংগুলি একসাথে লেগে থাকে - কোন আঠা প্রয়োজন নেই:

```kotlin
fun main() {
    val firstName = "Jet"
    val lastName = "Brains"
    val company = "Kotlin"

    // প্লাস অপারেটর ব্যবহার করে
    val fullName = firstName + " " + lastName 
    println(fullName) // আউটপুট: Jet Brains

    // স্ট্রিং টেম্পলেট ব্যবহার করে
    val employeeIntro = "Hi, I'm $firstName and I work at $company."
    println(employeeIntro) // আউটপুট: Hi, I'm Jet and I work at Kotlin.

    // concat() ফাংশন ব্যবহার করে
    val product = "IntelliJ IDEA"
    val description = " is awesome!"
    println(product.concat(description)) // আউটপুট: IntelliJ IDEA is awesome!
}
```

## গভীর ডাইভ

আমাদের স্ট্রিং জড়িয়ে রাখার মত পদার্থ থাকার সাথে সাথেই সংযোজনের প্রক্রিয়া চালু হয়েছে। প্রোগ্রামিং ভাষাগুলি এই কাজ সম্পাদনের পদ্ধতিতে ক্রমাগত বিবর্তন হয়েছে। প্রথম দিনগুলিতে, আপনি সাধারণত `+` অপারেটর দিয়ে একত্রিত পরিমাণের টেক্সট দেখতে পাবেন। আধুনিক Kotlin পর্যন্ত এসে, আপনি `$` প্রতীক সহ টেম্পলেট পেয়েছেন যা যাদুর মত স্ট্রিং ভেতরে ভ্যারিয়েবলগুলিকে টানে।

বিকল্প প্রচুর। যদি পারফরম্যান্স মুখ্য হয় এবং আপনি অনেকগুলি স্ট্রিং নিয়ে কাজ করছেন, StringBuilder আপনার সেরা বন্ধু হতে পারে, একাধিক স্ট্রিং অবজেক্টের সৃষ্টি এড়াতে। তারপর রয়েছে `joinToString` ফাংশন যা একটি তালিকাকে আপনার পছন্দের ডিলিমিটার দ্বারা পৃথক করে একসাথে মেশাতে সাহায্য করে।

প্রতিটি পদ্ধতির নিজস্ব কৌশল রয়েছে - `plus` ব্যবহারে সহজ কিন্তু ব্যবহার বেশি হলে ধীর হতে পারে; স্ট্রিং টেম্পলেট পাঠযোগ্যতা জন্য চমৎকার; `concat()` জাভার পদ্ধতির দিকে ফিরে যায় এবং একটু ঔপচারিক মনে হতে পারে; `StringBuilder` এবং `joinToString` দীর্ঘ অপারেশনের জন্য আরও কার্যকরী।

## আরও দেখুন

Kotlin স্ট্রিং পৃথিবীর সম্পর্কে আরও গভীরে ডাইভ করুন:

- [Kotlin ডকুমেন্টেশন: বেসিক টাইপস](https://kotlinlang.org/docs/basic-types.html#string-literals)

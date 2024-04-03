---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:16:17.932150-06:00
description: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u0996\u09CB\u0981\u099C\u09BE\
  \ \u098F\u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\u09A5\u09BE\u09AA\
  \u09A8 \u0995\u09B0\u09BE \u09AF\u09C7\u09A8 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09B2\u09C1\u0995\u09CB\u099A\u09C1\
  \u09B0\u09BF \u0996\u09C7\u09B2\u09BE, \u098F\u09B0\u09AA\u09B0 \u09B2\u09C1\u0995\
  \u09BF\u09DF\u09C7 \u09A5\u09BE\u0995\u09BE \u09AC\u09CD\u09AF\u0995\u09CD\u09A4\
  \u09BF\u0995\u09C7 \u0985\u09A8\u09CD\u09AF \u0995\u09BE\u09B0\u09CB \u09B8\u09BE\
  \u09A5\u09C7 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\u09BE\
  \u0964 \u098F\u099F\u09BF \u098F\u0995\u099F\u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\
  \u09A3 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982\u2026"
lastmod: '2024-03-17T18:47:43.977787-06:00'
model: gpt-4-0125-preview
summary: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u0996\u09CB\u0981\u099C\u09BE \u098F\
  \u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\u09A5\u09BE\u09AA\u09A8\
  \ \u0995\u09B0\u09BE \u09AF\u09C7\u09A8 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \ \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09B2\u09C1\u0995\u09CB\u099A\u09C1\u09B0\
  \u09BF \u0996\u09C7\u09B2\u09BE, \u098F\u09B0\u09AA\u09B0 \u09B2\u09C1\u0995\u09BF\
  \u09DF\u09C7 \u09A5\u09BE\u0995\u09BE \u09AC\u09CD\u09AF\u0995\u09CD\u09A4\u09BF\
  \u0995\u09C7 \u0985\u09A8\u09CD\u09AF \u0995\u09BE\u09B0\u09CB \u09B8\u09BE\u09A5\
  \u09C7 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\u09BE\u0964\
  \ \u098F\u099F\u09BF \u098F\u0995\u099F\u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982 \u0995\u09BE\
  \u099C, \u09AC\u09BE\u09B2\u09CD\u0995 \u098F\u09A1\u09BF\u099F\u09BF\u0982, \u09A1\
  \u09C7\u099F\u09BE \u09B8\u09CD\u09AF\u09BE\u09A8\u09BF\u099F\u09BE\u0987\u099C\u09C7\
  \u09B6\u09BE\u09A8 \u098F\u09AC\u0982 \u09AC\u09BF\u09B0\u0995\u09CD\u09A4\u09BF\
  \u0995\u09B0 \u0995\u09BE\u099C\u0997\u09C1\u09B2\u09BF \u0985\u099F\u09CB\u09AE\
  \u09C7\u099F \u0995\u09B0\u09BE\u09B0 \u09AE\u09A4\u09CB \u0995\u09BE\u099C\u09C7\
  \u09B0 \u099C\u09A8\u09CD\u09AF \u0985\u09AA\u09B0\u09BF\u09B9\u09BE\u09B0\u09CD\
  \u09AF\u0964."
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\u09A7\
  \u09BE\u09A8 \u098F\u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\u09A5\
  \u09BE\u09AA\u09A8"
weight: 10
---

## কিভাবে:
কোটলিন এর স্ট্যান্ডার্ড লাইব্রেরির মাধ্যমে টেক্সট ম্যানিপুলেশন সহজ হয়। নিচে দেখুন, কিভাবে `replace` ব্যবহার করে শব্দ পরিবর্তন করা যায়।

```kotlin
fun main() {
    val originalText = "Kotlin is fun, Kotlin is pragmatic!"
    val newText = originalText.replace("pragmatic", "cool")

    println(newText) // আউটপুট: Kotlin is fun, Kotlin is cool!
}
```

রেগেক্স প্যাটার্নের জন্য:

```kotlin
fun main() {
    val regex = "Kotlin".toRegex()
    val originalText = "Kotlin is fun, Kotlin is pragmatic!"
    val newText = regex.replace(originalText, "Java")

    println(newText) // আউটপুট: Java is fun, Java is pragmatic!
}
```

## গভীর ডাইভ
প্রিন্টের সময় থেকেই টেক্সট পুনর্লিখন চলে আসছে, কিন্তু প্রোগ্রামিং-এ, এর ব্যবহার শুরু হয় প্রাথমিক টেক্সট প্রসেসরের সাথে। বিকল্প? অবশ্যই – এডিটরের মধ্যে খুঁজে পাওয়া এবং প্রতিস্থাপন করার ফাংশন, `sed` এর মতো কমান্ড-লাইন টুলস। বিশেষ করে কোটলিনে, আপনার কাছে রেগেক্স এবং প্লেইন স্ট্রিং মেথড উপলব্ধ।

`replace` সহজ টেক্সটের জন্য সোজা; `Regex` আপনাকে প্যাটার্নের জন্য একটি সুইস আর্মি ছুরি প্রদান করে। রেগেক্স শক্তিশালী কিন্তু বিচক্ষণ – তারা প্যাটার্ন ম্যাচ করার জন্য বিশেষ সিনট্যাক্স ব্যবহার করে। রেগেক্স সম্পর্কে ভাবুন যেন আপনি ওয়াল্ডো খুঁজছেন, কিন্তু আপনি ওয়াল্ডো কি পরিধান করে তার নিয়ম তৈরি করছেন।

বাস্তবায়নের সতর্কতা? মনে রাখবেন, কোটলিনের `String` অপরিবর্তনীয়। টেক্সট পরিবর্তনের মেথড নতুন স্ট্রিং রিটার্ন করে; তারা মূল স্ট্রিং পরিবর্তন করে না।

## আরও দেখুন
- কোটলিন ডকুমেন্টেশন `replace` প্রসঙ্গে: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html
- কোটলিনে রেগেক্স: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/
- পুরানো ভালো `sed`: https://www.gnu.org/software/sed/manual/sed.html

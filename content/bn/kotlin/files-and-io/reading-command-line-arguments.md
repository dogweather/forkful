---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:10:05.360770-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u0989\u09AA\u09B0\u09C7\u09B0\
  \ \u0995\u09CB\u09A1\u09C7, `args` \u09B9\u09B2 \u098F\u0995\u099F\u09BF \u0985\u09CD\
  \u09AF\u09BE\u09B0\u09C7 \u09AF\u09BE \u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\
  \u09BE\u0987\u09A8 \u0986\u09B0\u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F\
  \ \u09A7\u09BE\u09B0\u09A3 \u0995\u09B0\u09C7\u0964 `main` \u09AB\u09BE\u0982\u09B6\
  \u09A8 \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\u09C7 \u09AF\u09C7\
  \ \u0986\u09AE\u09B0\u09BE \u0995\u09CB\u09A8 \u0986\u09B0\u09CD\u0997\u09C1\u09AE\
  \u09C7\u09A8\u09CD\u099F \u09AA\u09C7\u09DF\u09C7\u099B\u09BF \u0995\u09BF \u09A8\
  \u09BE, \u098F\u09AC\u0982 \u098F\u09B0 \u0985\u09A8\u09C1\u09B8\u09BE\u09B0\u09C7\
  \u2026"
lastmod: '2024-04-05T21:53:52.340126-06:00'
model: gpt-4-0125-preview
summary: "\u0989\u09AA\u09B0\u09C7\u09B0 \u0995\u09CB\u09A1\u09C7, `args` \u09B9\u09B2\
  \ \u098F\u0995\u099F\u09BF \u0985\u09CD\u09AF\u09BE\u09B0\u09C7 \u09AF\u09BE \u0995\
  \u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\u09B0\u09CD\u0997\
  \u09C1\u09AE\u09C7\u09A8\u09CD\u099F \u09A7\u09BE\u09B0\u09A3 \u0995\u09B0\u09C7\
  \u0964 `main` \u09AB\u09BE\u0982\u09B6\u09A8 \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\
  \u09BE \u0995\u09B0\u09C7 \u09AF\u09C7 \u0986\u09AE\u09B0\u09BE \u0995\u09CB\u09A8\
  \ \u0986\u09B0\u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F \u09AA\u09C7\u09DF\
  \u09C7\u099B\u09BF \u0995\u09BF \u09A8\u09BE, \u098F\u09AC\u0982 \u098F\u09B0 \u0985\
  \u09A8\u09C1\u09B8\u09BE\u09B0\u09C7 \u0985\u09AD\u09BF\u09AC\u09BE\u09A6\u09A8\
  \ \u099C\u09BE\u09A8\u09BE\u09DF\u0964."
title: "\u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\u09B0\
  \u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F\u0997\u09C1\u09B2\u09BF \u09AA\u09A1\
  \u09BC\u09BE"
weight: 23
---

## কিভাবে:
```kotlin
fun main(args: Array<String>) {
    if (args.isNotEmpty()) {
        println("Hello, ${args[0]}!")
    } else {
        println("Hello, unknown person!")
    }
}

// 'Kotlinista' আর্গুমেন্ট হিসাবে পাস করা হলে নমুনা আউটপুট:
// Hello, Kotlinista!
```

উপরের কোডে, `args` হল একটি অ্যারে যা কমান্ড লাইন আর্গুমেন্ট ধারণ করে। `main` ফাংশন পরীক্ষা করে যে আমরা কোন আর্গুমেন্ট পেয়েছি কি না, এবং এর অনুসারে অভিবাদন জানায়।

## গভীরে ডুব দিন
কমান্ড লাইন আর্গুমেন্টের ধারণা খুবই প্রাচীন; প্রোগ্রামিংয়ের শুরু থেকেই এটি ধরে রেখেছে - অথবা অন্তত ইন্টার‍্যাক্টিভ টার্মিনালগুলির সৃষ্টি থেকে। কোটলিনের প্রসঙ্গে, যা JVM এ চলে, কমান্ড লাইন আর্গুমেন্ট জাভার মতোই কার্যকারিতা দেয়।

অন্যান্য ভাষা অনুরূপ উপায় অফার করে, যেমন `argv` পাইথনে অথবা `$argc` এবং `$argv` PHP তে। কোটলিনের পদ্ধতি এটি সিম্পল রাখে - `main` ফাংশন শুধুমাত্র একটি `Array<String>` নেয়।

বাস্তবায়নের বিস্তারিত বিবরণের জন্য, মনে রাখা জরুরি যে অ্যারের ইনডেক্স শূন্য থেকে শুরু হয়। `args[0]` প্রথম আর্গুমেন্ট, `args[1]` দ্বিতীয়, এবং এভাবে চলে। এছাড়াও, মনে রাখবেন যে যদি আপনি একটি জটিল অ্যাপ বিল্ড করছেন যা আরও লচছিলভাবে কমান্ড পার্স করা দরকার, আপনি করলিঙ্গ্স-ক্লি এর মতো একটি নির্দিষ্ট লাইব্রেরির দিকে নজর দিতে পারেন।

## দেখুন এছাড়াও
- [কোটলিনের অফিসিয়াল ডকুমেন্টেশন কমান্ড-লাইন অ্যাপ্লিকেশনস উপরে](https://kotlinlang.org/docs/command-line.html)
- [GitHub এ করলিঙ্গ্স-ক্লি](https://github.com/Kotlin/kotlinx-cli)

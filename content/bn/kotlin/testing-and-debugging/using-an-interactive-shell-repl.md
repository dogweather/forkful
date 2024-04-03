---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:23:34.372394-06:00
description: "REPL (Read-Eval-Print Loop) \u09B9\u09B2 \u098F\u0995\u099F\u09BF \u09B8\
  \u09BE\u09A7\u09BE\u09B0\u09A3, \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u200C\u09CD\
  \u09AF\u09BE\u0995\u09CD\u099F\u09BF\u09AD \u0995\u09AE\u09CD\u09AA\u09BF\u0989\u099F\
  \u09BE\u09B0 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982\
  \ \u09AA\u09B0\u09BF\u09AC\u09C7\u09B6\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A6\u09CD\u09B0\u09C1\u09A4 \u0995\
  \u09CB\u09A1\u09BF\u0982 \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE, \u099F\u09C1\
  \u0995\u09B0\u09BE \u099F\u09C1\u0995\u09B0\u09BE \u0995\u09CB\u09A1 \u099F\u09C7\
  \u09B8\u09CD\u099F \u0995\u09B0\u09BE\u2026"
lastmod: '2024-03-17T18:47:43.994950-06:00'
model: gpt-4-0125-preview
summary: "REPL (Read-Eval-Print Loop) \u09B9\u09B2 \u098F\u0995\u099F\u09BF \u09B8\
  \u09BE\u09A7\u09BE\u09B0\u09A3, \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u200C\u09CD\
  \u09AF\u09BE\u0995\u09CD\u099F\u09BF\u09AD \u0995\u09AE\u09CD\u09AA\u09BF\u0989\u099F\
  \u09BE\u09B0 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982\
  \ \u09AA\u09B0\u09BF\u09AC\u09C7\u09B6\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A6\u09CD\u09B0\u09C1\u09A4 \u0995\
  \u09CB\u09A1\u09BF\u0982 \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE, \u099F\u09C1\
  \u0995\u09B0\u09BE \u099F\u09C1\u0995\u09B0\u09BE \u0995\u09CB\u09A1 \u099F\u09C7\
  \u09B8\u09CD\u099F \u0995\u09B0\u09BE \u0985\u09A5\u09AC\u09BE \u09AA\u09C2\u09B0\
  \u09CD\u09A3 \u0985\u09CD\u09AF\u09BE\u09AA\u09CD\u09B2\u09BF\u0995\u09C7\u09B6\u09A8\
  \ \u09A4\u09C8\u09B0\u09BF \u09A8\u09BE \u0995\u09B0\u09C7 \u098F\u0995\u099F\u09BF\
  \ \u09AD\u09BE\u09B7\u09BE\u09B0 \u09B8\u09BF\u09A8\u099F\u09CD\u09AF\u09BE\u0995\
  \u09CD\u09B8 \u09B6\u09C7\u0996\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u099F\
  \u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\u0964."
title: "\u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AF\u09BC\u09BE\u0995\u09CD\u099F\u09BF\
  \u09AD \u09B6\u09C7\u09B2 (REPL) \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09BE"
weight: 34
---

## কি এবং কেন?
REPL (Read-Eval-Print Loop) হল একটি সাধারণ, ইন্টার‌্যাক্টিভ কম্পিউটার প্রোগ্রামিং পরিবেশ। প্রোগ্রামাররা দ্রুত কোডিং পরীক্ষা, টুকরা টুকরা কোড টেস্ট করা অথবা পূর্ণ অ্যাপ্লিকেশন তৈরি না করে একটি ভাষার সিনট্যাক্স শেখার জন্য এটি ব্যবহার করে।

## কিভাবে:
Kotlin এর REPL চালু করা সহজ। আপনার টার্মিনাল খুলুন এবং `kotlinc` টাইপ করুন। আপনি Kotlin শেলে প্রবেশ করবেন। চলুন একটি ভেরিয়েবল নির্ধারণ করি এবং এর মান প্রিন্ট করা যাক:

```kotlin
স্বাগতম Kotlin সংস্করণ 1.7.10 (JRE 1.8.0_292-b10)
সাহায্যের জন্য :help টাইপ করুন, বের হতে :quit
>>> val greeting = "Hello, Kotlin REPL!"
>>> println(greeting)
Hello, Kotlin REPL!
```

## গভীর ডুব
Kotlin এর REPL ভাষার সাথে পরীক্ষা উৎসাহিত করার জন্য প্রদর্শিত হয়েছিল। এটি Python এর ইন্টার‌্যাক্টিভ শেলের মতো, তবে Kotlin এর সিনট্যাক্স এবং বিশেষত্বগুলির জন্য নির্মিত। বিকল্প? IntelliJ IDEA এর মতো IDEs এ ইন্টার‌্যাক্টিভ পরিবেশ, এবং অনলাইন Kotlin খেলার মাঠ। REPL মূহুর্তের জন্য কোড কম্পাইল করে, শিক্ষা এবং ডিবাগিং এর জন্য তাৎক্ষণিক প্রতিক্রিয়া প্রদান করে।

## দেখুন এও
- Kotlin ডকুমেন্টেশন এ REPL এর উপর: [https://kotlinlang.org/docs/command-line.html#run-the-repl](https://kotlinlang.org/docs/command-line.html#run-the-repl)
- ব্রাউজারে Kotlin চেষ্টা করুন: [https://play.kotlinlang.org](https://play.kotlinlang.org)
- IntelliJ IDEA জন্য JetBrains Kotlin Playground প্লাগিন।

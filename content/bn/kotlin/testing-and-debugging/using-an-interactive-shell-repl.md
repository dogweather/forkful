---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:23:34.372394-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Kotlin \u098F\u09B0 REPL \u099A\
  \u09BE\u09B2\u09C1 \u0995\u09B0\u09BE \u09B8\u09B9\u099C\u0964 \u0986\u09AA\u09A8\
  \u09BE\u09B0 \u099F\u09BE\u09B0\u09CD\u09AE\u09BF\u09A8\u09BE\u09B2 \u0996\u09C1\
  \u09B2\u09C1\u09A8 \u098F\u09AC\u0982 `kotlinc` \u099F\u09BE\u0987\u09AA \u0995\u09B0\
  \u09C1\u09A8\u0964 \u0986\u09AA\u09A8\u09BF Kotlin \u09B6\u09C7\u09B2\u09C7 \u09AA\
  \u09CD\u09B0\u09AC\u09C7\u09B6 \u0995\u09B0\u09AC\u09C7\u09A8\u0964 \u099A\u09B2\
  \u09C1\u09A8 \u098F\u0995\u099F\u09BF \u09AD\u09C7\u09B0\u09BF\u09AF\u09BC\u09C7\
  \u09AC\u09B2 \u09A8\u09BF\u09B0\u09CD\u09A7\u09BE\u09B0\u09A3 \u0995\u09B0\u09BF\
  \ \u098F\u09AC\u0982 \u098F\u09B0\u2026"
lastmod: '2024-03-17T18:47:43.994950-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u098F\u09B0 REPL \u099A\u09BE\u09B2\u09C1 \u0995\u09B0\u09BE \u09B8\
  \u09B9\u099C\u0964 \u0986\u09AA\u09A8\u09BE\u09B0 \u099F\u09BE\u09B0\u09CD\u09AE\
  \u09BF\u09A8\u09BE\u09B2 \u0996\u09C1\u09B2\u09C1\u09A8 \u098F\u09AC\u0982 `kotlinc`\
  \ \u099F\u09BE\u0987\u09AA \u0995\u09B0\u09C1\u09A8\u0964 \u0986\u09AA\u09A8\u09BF\
  \ Kotlin \u09B6\u09C7\u09B2\u09C7 \u09AA\u09CD\u09B0\u09AC\u09C7\u09B6 \u0995\u09B0\
  \u09AC\u09C7\u09A8\u0964 \u099A\u09B2\u09C1\u09A8 \u098F\u0995\u099F\u09BF \u09AD\
  \u09C7\u09B0\u09BF\u09AF\u09BC\u09C7\u09AC\u09B2 \u09A8\u09BF\u09B0\u09CD\u09A7\u09BE\
  \u09B0\u09A3 \u0995\u09B0\u09BF \u098F\u09AC\u0982 \u098F\u09B0 \u09AE\u09BE\u09A8\
  \ \u09AA\u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09BE \u09AF\u09BE\u0995\
  ."
title: "\u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AF\u09BC\u09BE\u0995\u09CD\u099F\u09BF\
  \u09AD \u09B6\u09C7\u09B2 (REPL) \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09BE"
weight: 34
---

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

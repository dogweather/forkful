---
title:                "ইন্টারয়াক্টিভ শেল (REPL) ব্যবহার করা"
date:                  2024-03-17T18:23:34.372394-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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

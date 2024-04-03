---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:15:02.380867-06:00
description: "\u0995\u09C0\u09AD\u09BE\u09AC\u09C7: \u0995\u099F\u09B2\u09BF\u09A8\
  \u09C7, `roundToInt()`, `roundToDouble()`, \u098F\u09AC\u0982 \u0985\u09A7\u09BF\
  \u0995 \u09A8\u09BF\u09DF\u09A8\u09CD\u09A4\u09CD\u09B0\u09A3\u09C7\u09B0 \u099C\
  \u09A8\u09CD\u09AF `BigDecimal` \u098F\u09B0 \u09AE\u09A4\u09CB \u09AC\u09C7\u09B6\
  \ \u0995\u09BF\u099B\u09C1 \u09AB\u09BE\u0982\u09B6\u09A8\u09C7\u09B0 \u09AE\u09BE\
  \u09A7\u09CD\u09AF\u09AE\u09C7 \u0997\u09CB\u09B2\u0995\u09B0\u09A3 \u0995\u09B0\
  \u09BE \u09AF\u09C7\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7."
lastmod: '2024-03-17T18:47:43.988208-06:00'
model: gpt-4-0125-preview
summary: "\u0995\u099F\u09B2\u09BF\u09A8\u09C7, `roundToInt()`, `roundToDouble()`,\
  \ \u098F\u09AC\u0982 \u0985\u09A7\u09BF\u0995 \u09A8\u09BF\u09DF\u09A8\u09CD\u09A4\
  \u09CD\u09B0\u09A3\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF `BigDecimal` \u098F\u09B0\
  \ \u09AE\u09A4\u09CB \u09AC\u09C7\u09B6 \u0995\u09BF\u099B\u09C1 \u09AB\u09BE\u0982\
  \u09B6\u09A8\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u0997\u09CB\
  \u09B2\u0995\u09B0\u09A3 \u0995\u09B0\u09BE \u09AF\u09C7\u09A4\u09C7 \u09AA\u09BE\
  \u09B0\u09C7."
title: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09A8\u09BF\u09B0\u09CD\u09A3\u09DF"
weight: 13
---

## কীভাবে:
কটলিনে, `roundToInt()`, `roundToDouble()`, এবং অধিক নিয়ন্ত্রণের জন্য `BigDecimal` এর মতো বেশ কিছু ফাংশনের মাধ্যমে গোলকরণ করা যেতে পারে:

```kotlin
fun main() {
    val number1 = 3.14159
    println(number1.roundToInt()) // আউটপুট: 3

    val number2 = 3.5
    println(number2.roundToInt()) // আউটপুট: 4

    val number3 = 123.456
    println("%.2f".format(number3)) // আউটপুট: 123.46
    
    val bigDecimal = number3.toBigDecimal().setScale(1, RoundingMode.HALF_EVEN)
    println(bigDecimal) // আউটপুট: 123.5
}
```

## গভীর ডুব
ঐতিহাসিকভাবে, সংখ্যা গোলকরণ গণিত এবং কম্পিউটেশন উভয় ক্ষেত্রেই একটি মৌলিক ধারণা, যা সংখ্যাগত নির্ভুলতার সীমাবদ্ধতা সামাল দেবার জন্য তৈরি হয়েছে। প্রাথমিক কম্পিউটিংয়ে, স্মৃতির উচ্চ মূল্যের কারণে গোলকরণ অত্যন্ত জরুরি ছিল।

কটলিনে, গোলকরণ স্ট্যান্ডার্ড জাভা লাইব্রেরির উপর ভিত্তি করে। গোলকরণের জন্য অপশনগুলো অন্তর্ভুক্ত করে `Math.round()`, যা নিকটতম পূর্ণ সংখ্যায় গোলকরণ করে, এবং `BigDecimal` নিজের মত করে গোলকরণের জন্য, যেখানে আপনি একটি স্কেল এবং একটি `RoundingMode` নির্ধারণ করতে পারেন। 

প্রতিটি `RoundingMode` বেঁধে দেয় ভিন্ন নীতি যখন গণনা ঠিক মাঝামাঝি একটি সংখ্যায় আটকে যায়। যেমন, `RoundingMode.HALF_UP` নিকটতম প্রতিবেশীর দিকে গোলকরণ করে, প্রতিবেশী দুটি সমান দূরত্বে থাকলে এটি উপরের দিকে গোলকরণ করে।

## আরও দেখুন
- কটলিন ডকুমেন্টেশনে [`BigDecimal`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/java.math.-big-decimal/index.html) সম্পর্কে
- অরাকলের জাভা ডকুমেন্টেশনে [`RoundingMode`](https://docs.oracle.com/javase/8/docs/api/java/math/RoundingMode.html) সম্পর্কে
- ফ্লোটিং-পয়েন্ট অ্যারিথমেটিকের জন্য IEEE স্ট্যান্ডার্ড (IEEE 754) [IEEE স্ট্যান্ডার্ড 754](https://ieeexplore.ieee.org/document/4610935)

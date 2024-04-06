---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:30.785797-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09A8\u09AE\u09C1\u09A8\u09BE\
  \ \u0986\u0989\u099F\u09AA\u09C1\u099F."
lastmod: '2024-04-05T21:53:52.335405-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u09A4\u09C1\u09B2\
  \u09A8\u09BE \u0995\u09B0\u09BE"
weight: 27
---

## কিভাবে:
```Kotlin
আমদানি java.time.LocalDate

মুখ্য ফাংশন() {
    ভ্যাল date1 = LocalDate.of(2023, 4, 10)
    ভ্যাল date2 = LocalDate.of(2023, 5, 15)

    println(date1.isBefore(date2))  // সত্য
    println(date1.isAfter(date2))   // মিথ্যা
    println(date1.isEqual(date2))   // মিথ্যা

    // compareTo ব্যবহার করে তুলনা
    println(date1.compareTo(date2)) // যদি date1 date2 এর আগে হয় তাহলে -1
}
```

নমুনা আউটপুট:

```
সত্য
মিথ্যা
মিথ্যা
-1
```

## গভীরে ডাইভ
ঐতিহাসিকভাবে, জাভা `Date` এবং `Calendar` শ্রেণী সরবরাহ করেছে কিন্তু তারা খুব ব্যবহারকারী-বান্ধব ছিল না। কোটলিন এর অধীনে অনুরূপ শ্রেণী ব্যবহার করে কিন্তু জাভা ৮ এ প্রবর্তিত `java.time` প্যাকেজ ব্যবহার করার জন্য উৎসাহিত করে, যা বেশি স্পষ্টতা এবং উপযোগিতা প্রদান করে।

এল্টারনেটিভ হিসেবে `Instant` টাইমস্ট্যাম্পের জন্য, `ZonedDateTime` সময়-জোন নির্দিষ্ট তারিখের জন্য, অথবা জোডা-টাইম মত তৃতীয়-পক্ষের লাইব্রেরি ব্যবহার করা যেতে পারে। বাস্তবায়নের বিস্তারিত মনে রাখা—`Instant` একটি পারম্পরিক Unix টাইমস্ট্যাম্প ব্যবহার করে যেখানে `LocalDate` এটিকে লুকানো এবং সময় বা সময় অঞ্চল ছাড়া একটি ধারণাগত দিনের সাথে ব্যবহার করে।

কোন শ্রেণী আপনার চাহিদা সবচেয়ে ভালোভাবে পূরণ করে তা জানা জরুরি। `LocalDate` বেশিরভাগ তারিখের তুলনা জন্য ঠিক আছে, কিন্তু নির্দিষ্ট মুহূর্তের তুলনার জন্য, `ZonedDateTime` বা `Instant` বিবেচনা করুন।

## আরও দেখুন
- তারিখ এবং সময় উপর অফিসিয়াল কোটলিন ডকুমেন্টেশন: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/)
- জাভা ৮ তারিখ এবং সময় গাইড: [https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)
- জোডা-টাইম লাইব্রেরি: [https://www.joda.org/joda-time/](https://www.joda.org/joda-time/)

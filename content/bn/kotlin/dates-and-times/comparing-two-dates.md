---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:30.785797-06:00
description: "\u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\u09B0\
  \ \u09A4\u09C1\u09B2\u09A8\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF\
  \ \u0985\u09AA\u09B0\u099F\u09BF\u09B0 \u0986\u0997\u09C7 \u0995\u09BF\u09A8\u09BE\
  \ \u09AC\u09BE \u09AA\u09B0\u09C7 \u0995\u09BF\u09A8\u09BE, \u0985\u09A5\u09AC\u09BE\
  \ \u09A4\u09BE\u09B0\u09BE \u09AF\u09A6\u09BF \u098F\u0995\u0987 \u09AE\u09C1\u09B9\
  \u09C2\u09B0\u09CD\u09A4\u09C7 \u09A5\u09BE\u0995\u09C7 \u09A4\u09BE \u09AF\u09BE\
  \u099A\u09BE\u0987 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u0987 \u0995\u09BE\u099C \u0995\
  \u09B0\u09C7 \u09AF\u09C7\u09AE\u09A8 \u0987\u09AD\u09C7\u09A8\u09CD\u099F \u09B8\
  \u09BE\u099C\u09BE\u09A8\u09CB,\u2026"
lastmod: '2024-03-17T18:47:44.006894-06:00'
model: gpt-4-0125-preview
summary: "\u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\u09B0 \u09A4\
  \u09C1\u09B2\u09A8\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u0985\
  \u09AA\u09B0\u099F\u09BF\u09B0 \u0986\u0997\u09C7 \u0995\u09BF\u09A8\u09BE \u09AC\
  \u09BE \u09AA\u09B0\u09C7 \u0995\u09BF\u09A8\u09BE, \u0985\u09A5\u09AC\u09BE \u09A4\
  \u09BE\u09B0\u09BE \u09AF\u09A6\u09BF \u098F\u0995\u0987 \u09AE\u09C1\u09B9\u09C2\
  \u09B0\u09CD\u09A4\u09C7 \u09A5\u09BE\u0995\u09C7 \u09A4\u09BE \u09AF\u09BE\u099A\
  \u09BE\u0987 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u0987 \u0995\u09BE\u099C \u0995\u09B0\
  \u09C7 \u09AF\u09C7\u09AE\u09A8 \u0987\u09AD\u09C7\u09A8\u09CD\u099F \u09B8\u09BE\
  \u099C\u09BE\u09A8\u09CB, \u09B8\u09BF\u09A1\u09BF\u0989\u09B2\u09BF\u0982, \u098F\
  \u09AC\u0982 \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\u09B0 \u09AE\u09A7\u09CD\u09AF\
  \u09C7 \u09B8\u09AE\u09AF\u09BC\u09C7\u09B0 \u09AC\u09CD\u09AF\u09AC\u09A7\u09BE\
  \u09A8 \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\u09BE\u09B0 \u099C\
  \u09A8\u09CD\u09AF\u0964."
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

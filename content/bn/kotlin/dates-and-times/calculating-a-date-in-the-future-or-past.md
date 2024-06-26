---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:58.194009-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Kotlin \u09A4\u09BE\u09B0\u09BF\
  \u0996 \u098F\u09AC\u0982 \u09B8\u09AE\u09AF\u09BC \u09A8\u09BF\u09AF\u09BC\u09A8\
  \u09CD\u09A4\u09CD\u09B0\u09A3 \u0995\u09B0\u09C7 `java.time` \u09B2\u09BE\u0987\
  \u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09C7\u0964 \u09A6\u09BF\u09A8 \u09AF\u09CB\u0997 \u09AC\u09BE \u09AC\
  \u09BF\u09AF\u09BC\u09CB\u0997 \u0995\u09B0\u09A4\u09C7, `plusDays()` \u09AC\u09BE\
  \ `minusDays()` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C1\u09A8\
  \u0964 \u098F\u0996\u09BE\u09A8\u09C7\u2026"
lastmod: '2024-03-17T18:47:44.007842-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u09A4\u09BE\u09B0\u09BF\u0996 \u098F\u09AC\u0982 \u09B8\u09AE\u09AF\
  \u09BC \u09A8\u09BF\u09AF\u09BC\u09A8\u09CD\u09A4\u09CD\u09B0\u09A3 \u0995\u09B0\
  \u09C7 `java.time` \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\u0964 \u09A6\u09BF\u09A8\
  \ \u09AF\u09CB\u0997 \u09AC\u09BE \u09AC\u09BF\u09AF\u09BC\u09CB\u0997 \u0995\u09B0\
  \u09A4\u09C7, `plusDays()` \u09AC\u09BE `minusDays()` \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09C1\u09A8\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\
  \u0995\u099F\u09BF \u09B8\u0982\u0995\u09CD\u09B7\u09BF\u09AA\u09CD\u09A4 \u09AC\
  \u09B0\u09CD\u09A3\u09A8\u09BE \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2\
  ."
title: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u09AC\u09BE \u0985\u09A4\u09C0\
  \u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u0997\u09A3\u09A8\u09BE \u0995\
  \u09B0\u09BE"
weight: 26
---

## কিভাবে:
Kotlin তারিখ এবং সময় নিয়ন্ত্রণ করে `java.time` লাইব্রেরি ব্যবহার করে। দিন যোগ বা বিয়োগ করতে, `plusDays()` বা `minusDays()` ব্যবহার করুন। এখানে একটি সংক্ষিপ্ত বর্ণনা দেওয়া হল:

```kotlin
import java.time.LocalDate

fun main() {
    val today = LocalDate.now()
    val tenDaysLater = today.plusDays(10)
    val tenDaysBefore = today.minusDays(10)
    
    println("আজ: $today")
    println("আজ থেকে দশ দিন পরে: $tenDaysLater")
    println("দশ দিন আগে: $tenDaysBefore")
}
```

নমুনা আউটপুট:

```
আজ: 2023-03-15
আজ থেকে দশ দিন পরে: 2023-03-25
দশ দিন আগে: 2023-03-05
```

দিনগুলির পরে, আপনি মাস এবং বছরের সাথেও খেলতে পারেন (`plusMonths()`, `minusMonths()`, `plusYears()`, `minusYears()`).

## গভীর ডুব
তারিখ হিসাব নতুন কিছু নয়। জাভা 8 থেকে, `java.time` প্যাকেজ তারিখ-সময় অঙ্কনের জন্য পছন্দ হয়ে গেছে—পুরানো `Calendar` বা `Date` এর চেয়ে অনেক ভালো, যা ছিল জটিল এবং থ্রেড-সেফ নয়।

`java.time` অপরিবর্তনীয় অবজেক্ট ব্যবহার করে, তাই ভুলবশত আপনার তারিখগুলি পরিবর্তন করার ফলে দুর্ঘটনাকরা বাগ থেকে আপনি বাঁচতে পারেন। `LocalDate`, `LocalTime`, `LocalDateTime`, এবং `ZonedDateTime` এর মতো অবজেক্টগুলি সময়ের বিভিন্ন দিক নির্ভুলভাবে প্রতিনিধিত্ব করতে আপনাকে সাহায্য করে।

বিকল্প? অবশ্যই আছে। `java.time` এর আগে, Joda-Time ছিল পছন্দের অস্ত্র। কিছু পুরোনো সিস্টেমে এটি এখনও ব্যবহৃত হয়। এবং Android পরিস্থিতিতে, ThreeTenABP লাইব্রেরি Java 6 & 7 এর সাথে সম্মতির জন্য `java.time` বৈশিষ্ট্যগুলি ব্যাকপোর্ট করে।

`java.time` API এছাড়াও টাইমজোন-সচেতন ডিজাইন করা হয়েছে, `ZonedDateTime` এর মতো ক্লাসের সাথে ধন্যবাদ। তাই যখন আপনি তারিখগুলি চালাচালি করছেন, আপনি পৃথিবীর ঘূর্ণনের ইতিহাসকে সম্মান করতে পারেন।

## আরও দেখুন
- অফিসিয়াল `java.time` ডকুমেন্টেশন: [Java SE Date Time](https://docs.oracle.com/javase/tutorial/datetime/)
- Android ডেভেলপারদের জন্য, `ThreeTenABP` লাইব্রেরির বিস্তারিত জানার জন্য: [ThreeTenABP on GitHub](https://github.com/JakeWharton/ThreeTenABP)
- তারিখ এবং সময় সম্পর্কে আরও গভীর জ্ঞান নিতে চাইলে, একটি বিস্তারিত গাইড: [Java-তে তারিখ এবং সময়](https://www.baeldung.com/java-8-date-time-intro)

---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:45.573506-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u0995\u09CB\u099F\u09B2\u09BF\
  \u09A8\u09C7, \u0986\u09AA\u09A8\u09BF `Date` \u0995\u09C7 `String` \u098F \u09B0\
  \u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09A4\u09C7 `SimpleDateFormat`\
  \ \u0995\u09CD\u09B2\u09BE\u09B8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u0986\u09B8\u09C1\u09A8\
  \ \u0995\u09BF\u099B\u09C1 \u0995\u09CB\u09A1 \u09B2\u09BF\u0996\u09BF."
lastmod: '2024-03-17T18:47:44.005931-06:00'
model: gpt-4-0125-preview
summary: "\u0995\u09CB\u099F\u09B2\u09BF\u09A8\u09C7, \u0986\u09AA\u09A8\u09BF `Date`\
  \ \u0995\u09C7 `String` \u098F \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0\
  \ \u0995\u09B0\u09A4\u09C7 `SimpleDateFormat` \u0995\u09CD\u09B2\u09BE\u09B8 \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\
  \u09C7\u09A8\u0964 \u0986\u09B8\u09C1\u09A8 \u0995\u09BF\u099B\u09C1 \u0995\u09CB\
  \u09A1 \u09B2\u09BF\u0996\u09BF."
title: "\u09A4\u09BE\u09B0\u09BF\u0996\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u098F \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE"
weight: 28
---

## কিভাবে:
কোটলিনে, আপনি `Date` কে `String` এ রূপান্তর করতে `SimpleDateFormat` ক্লাস ব্যবহার করতে পারেন। আসুন কিছু কোড লিখি:

```kotlin
import java.text.SimpleDateFormat
import java.util.Date

fun main() {
    val date = Date() // বর্তমান সময়ের জন্য একটি Date অবজেক্ট তৈরি করুন
    val format = SimpleDateFormat("yyyy-MM-dd HH:mm:ss") // তারিখের প্যাটার্ন নির্ধারণ করুন
    val dateString = format.format(date) // Date কে String এ রূপান্তর করুন
    println(dateString) // তারিখের স্ট্রিং আউটপুট করুন
}
```

নমুনা আউটপুট এরকম হতে পারে:

```
2023-03-25 14:45:32
```

## গভীর ডুব
`java.time` এর আগমনের আগে, জাভা এবং তদ্বারা কোটলিনে তারিখ-স্ট্রিং রূপান্তরের জন্য `SimpleDateFormat` ছিল প্রাথমিক পছন্দ। হ্যাঁ, কোটলিন জাভা ভার্চুয়াল মেশিনে চলে এবং জাভা লাইব্রেরিগুলির সাথে সহজেই মিশে যায়।

তবে, জাভা 8 এর সাথে সাথে, `java.time` দৃশ্যপটে এসে পড়ে, `DateTimeFormatter` নিয়ে আসে যা অনেক উন্নত API সাথে আসে। এটি একটি গেম-চেঞ্জার ছিল, নিরাপদ, অপরিবর্তনীয়, এবং থ্রেড-সেফ তারিখ-সময় পরিচালনা অফার করে। কোটলিনের এর জন্য নেটিভ সাপোর্ট আছে:

```kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun main() {
    val currentDate = LocalDateTime.now() // বর্তমান তারিখ এবং সময় পান
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    val formattedDate = currentDate.format(formatter)
    println(formattedDate)
}
```

বিকল্প? অবশ্যই। অ-মানক প্রয়োজনীয়তার জন্য অথবা বিভিন্ন তারিখ লাইব্রেরির মধ্যে জ্লগিংয়ের জন্য, তৃতীয়-পক্ষের অপশনের মধ্যে Joda-Time সোনার মান ছিল। বর্তমানে, `java.time` বেশিরভাগ ভিত্তি কভার করে।

বাস্তবায়নের বিস্তারিত অনুসারে, `SimpleDateFormat` থ্রেড-সেফ নয়, অর্থাৎ এটি যখন সহযোগী পরিবেশে ব্যবহৃত হয় তখন এটি সমস্যায় পড়তে পারে। `DateTimeFormatter` এর এই সমস্যা নেই। একবার তৈরি করুন, চিরস্থায়ীভাবে বা অন্তত আপনার অ্যাপ্লিকেশনের জুড়ে ব্যবহার করুন এবং বেশি চিন্তা না করুন।

## আরও দেখুন
- `DateTimeFormatter` JavaDoc আপনার সমস্ত প্যাটার্ন প্রয়োজনীয়তার জন্য: [DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- যদি আপনি নস্টালজিক অনুভব করেন অথবা পুরাতন সিস্টেমের জন্য উদাহরণ প্রয়োজন হয়, তাহলে এখানে `SimpleDateFormat` সম্পর্কে তথ্য: [SimpleDateFormat](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)

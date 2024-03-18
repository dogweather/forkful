---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:45.573506-06:00
description: "\u098F\u0995\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996\u0995\u09C7\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09AF\u09BC\u09C7 \u09B0\u09C2\u09AA\
  \u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09AC\
  \u09BF\u09B6\u09C7\u09B7 \u098F\u0995\u099F\u09BF \u09B8\u09AE\u09AF\u09BC\u0995\
  \u09C7 \u09AE\u09BE\u09A8\u09C1\u09B7\u09C7\u09B0 \u09AA\u09A1\u09BC\u09A4\u09C7\
  \ \u09B8\u0995\u09CD\u09B7\u09AE \u09AB\u09B0\u09CD\u09AE\u09CD\u09AF\u09BE\u099F\
  \u09C7 \u09AA\u09CD\u09B0\u09A6\u09B0\u09CD\u09B6\u09A8 \u0995\u09B0\u09BE\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u09A4\u09BE\u09B0\u09BF\u0996\u0997\u09C1\u09B2\u09BF \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0\u0995\u09BE\u09B0\u09C0\u09A6\u09C7\u09B0 \u09AA\u09CD\u09B0\u09A6\
  \u09B0\u09CD\u09B6\u09A8\u2026"
lastmod: '2024-03-17T18:47:44.005931-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996\u0995\u09C7 \u09B8\
  \u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09AF\u09BC\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\
  \u09CD\u09A4\u09B0 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09AC\u09BF\u09B6\
  \u09C7\u09B7 \u098F\u0995\u099F\u09BF \u09B8\u09AE\u09AF\u09BC\u0995\u09C7 \u09AE\
  \u09BE\u09A8\u09C1\u09B7\u09C7\u09B0 \u09AA\u09A1\u09BC\u09A4\u09C7 \u09B8\u0995\
  \u09CD\u09B7\u09AE \u09AB\u09B0\u09CD\u09AE\u09CD\u09AF\u09BE\u099F\u09C7 \u09AA\
  \u09CD\u09B0\u09A6\u09B0\u09CD\u09B6\u09A8 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A4\u09BE\u09B0\
  \u09BF\u0996\u0997\u09C1\u09B2\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\
  \u09BE\u09B0\u09C0\u09A6\u09C7\u09B0 \u09AA\u09CD\u09B0\u09A6\u09B0\u09CD\u09B6\u09A8\
  \u2026"
title: "\u09A4\u09BE\u09B0\u09BF\u0996\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u098F \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
একটি তারিখকে স্ট্রিংয়ে রূপান্তর করা মানে বিশেষ একটি সময়কে মানুষের পড়তে সক্ষম ফর্ম্যাটে প্রদর্শন করা। প্রোগ্রামাররা তারিখগুলি ব্যবহারকারীদের প্রদর্শন অথবা তারা সঞ্চয় এবং ডেটা ট্রান্সফারের জন্য তারিখগুলিকে সিরিয়ালাইজ করার জন্য এটি করে।

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

---
title:                "স্ট্রিং থেকে তারিখ পার্স করা"
date:                  2024-03-17T18:06:20.753185-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কী এবং কেন?
একটি তারিখকে স্ট্রিং থেকে পার্স করা মানে টেক্সটকে একটি তারিখ অবজেক্টে রূপান্তরিত করা। এই কাজটি মৌলিক হয় যেসব অ্যাপ্লিকেশন ব্যবহারকারীদের দ্বারা প্রবেশ করানো অথবা বাহ্যিক ডেটাসেট থেকে সংগৃহীত তারিখগুলির সাথে সংযোগ সাধন করে, এটি তারিখগুলিকে সহজেই ম্যানিপুলেট এবং প্রয়োজন অনুসারে বিন্যাস করা সম্ভব করে।

## কীভাবে:
Kotlin জাভা ৮-এ চালু হওয়া `java.time` প্যাকেজের মাধ্যমে তারিখ পার্সিং সাপোর্ট করে। এখানে `LocalDateTime` এবং একটি নির্দিষ্ট প্যাটার্ন ব্যবহার করে একটি সহজ প্রক্রিয়া উপস্থাপিত হল:

```kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun parseDateFromString(dateString: String): LocalDateTime {
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    return LocalDateTime.parse(dateString, formatter)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val date = parseDateFromString(dateString)
    println(date)  // আউটপুট: 2023-04-01T12:00
}
```

আরও নমনীয়তা পেতে বা এপিআইগুলির মতো বাহ্যিক সোর্স থেকে তারিখ হ্যান্ডেল করতে, আপনি Joda-Time এর মতো তৃতীয় পক্ষের লাইব্রেরি ব্যবহার করতে পারেন (যদিও `java.time` দৃঢ় থাকায় এটি এখন কম সাধারণ)। তবে, অধিকাংশ Kotlin অ্যাপ্লিকেশনের জন্য জেডিকে দ্বারা প্রদত্ত আধুনিক পদ্ধতি অনুসরণ করা পছন্দনীয়।

জাভা ৮ এর আগের সংস্করণগুলির জন্য বা এন্ড্রয়েড API লেভেলগুলিতে `java.time` সমর্থন না থাকার কারণে তৃতীয় পক্ষের লাইব্রেরি ব্যবহার না করে Kotlin-এ তারিখ পার্স করতে, আপনি এছাড়াও `SimpleDateFormat` ক্লাসের ব্যবহার করতে পারেন:

```kotlin
import java.text.SimpleDateFormat

fun parseDateUsingSimpleDateFormat(dateString: String): java.util.Date {
    val formatter = SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    return formatter.parse(dateString)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val date = parseDateUsingSimpleDateFormat(dateString)
    println(date)  // আউটপুট আপনার টাইমজোনের উপর নির্ভর করবে, উদাঃ, Sat Apr 01 12:00:00 GMT 2023
}
```

`SimpleDateFormat` ব্যবহার করার সময় সবসময় টাইমজোন সেট করতে ভুলবেন না, কারণ পার্স করা তারিখে অনাকাঙ্ক্ষিত অফসেট এড়াতে।

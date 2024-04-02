---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:27:18.143513-06:00
description: "\u09A8\u09BF\u09AF\u09BC\u09AE\u09BF\u09A4 \u0985\u09AD\u09BF\u09AC\u09CD\
  \u09AF\u0995\u09CD\u09A4\u09BF (regex) \u09B9\u09B2 \u098F\u0995\u099F\u09BF \u09B6\
  \u0995\u09CD\u09A4\u09BF\u09B6\u09BE\u09B2\u09C0 \u099F\u09C7\u0995\u09CD\u09B8\u099F\
  \ \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u0995\u09B0\u09A3\
  \ \u099F\u09C2\u09B2, \u09AF\u09BE \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\
  \u09AE\u09BE\u09B0\u09A6\u09C7\u09B0 \u0989\u09A8\u09CD\u09A8\u09A4 \u09AA\u09CD\
  \u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8-\u09AE\u09CD\u09AF\u09BE\u099A\u09BF\u0982\
  \ \u0995\u09CC\u09B6\u09B2\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0996\u09CB\u0981\u099C\u09BE, \u09AE\
  \u09BF\u09B2 \u0996\u09CB\u0981\u099C\u09BE\u2026"
lastmod: '2024-03-17T18:47:43.983350-06:00'
model: gpt-4-0125-preview
summary: "\u09A8\u09BF\u09AF\u09BC\u09AE\u09BF\u09A4 \u0985\u09AD\u09BF\u09AC\u09CD\
  \u09AF\u0995\u09CD\u09A4\u09BF (regex) \u09B9\u09B2 \u098F\u0995\u099F\u09BF \u09B6\
  \u0995\u09CD\u09A4\u09BF\u09B6\u09BE\u09B2\u09C0 \u099F\u09C7\u0995\u09CD\u09B8\u099F\
  \ \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u0995\u09B0\u09A3\
  \ \u099F\u09C2\u09B2, \u09AF\u09BE \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\
  \u09AE\u09BE\u09B0\u09A6\u09C7\u09B0 \u0989\u09A8\u09CD\u09A8\u09A4 \u09AA\u09CD\
  \u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8-\u09AE\u09CD\u09AF\u09BE\u099A\u09BF\u0982\
  \ \u0995\u09CC\u09B6\u09B2\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0996\u09CB\u0981\u099C\u09BE, \u09AE\
  \u09BF\u09B2 \u0996\u09CB\u0981\u099C\u09BE\u2026"
title: "\u09B0\u09C7\u0997\u09C1\u09B2\u09BE\u09B0 \u098F\u0995\u09CD\u09B8\u09AA\u09CD\
  \u09B0\u09C7\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09BE"
weight: 11
---

## কি এবং কেন?

নিয়মিত অভিব্যক্তি (regex) হল একটি শক্তিশালী টেক্সট প্রক্রিয়াকরণ টূল, যা প্রোগ্রামারদের উন্নত প্যাটার্ন-ম্যাচিং কৌশলের মাধ্যমে স্ট্রিং খোঁজা, মিল খোঁজা এবং পরিবর্তন করার অনুমতি দেয়। Kotlin-এ, regex ব্যবহার করে ভ্যালিডেশন, পার্সিং, অথবা রূপান্তরকরণের মত জটিল টেক্সট প্রক্রিয়াকরণ কাজগুলি কার্যকরীভাবে সম্পাদন করা যায়, যা সাধারণ স্ট্রিং পরিবর্তন থেকে জটিল টেক্সট বিশ্লেষণের কাজ পর্যন্ত অপরিহার্য করে তোলে।

## কিভাবে:

### বেসিক ম্যাচিং
Kotlin-এ একটি স্ট্রিং নির্দিষ্ট প্যাটার্নের সাথে মিলে কিনা চেক করতে, `Regex` ক্লাসের `matches` মেথড ব্যবহার করা যায়।

```kotlin
val pattern = "kotlin".toRegex()
val input = "I love kotlin"
val result = pattern.containsMatchIn(input)

println(result)  // আউটপুট: true
```

### স্ট্রিং-এর অংশ খুঁজে পেতে ও বের করে আনা
আপনি যদি একটি স্ট্রিং-এর যে অংশগুলো একটি প্যাটার্নের সাথে মিলে যায় সেগুলো খুঁজে বের করতে চান, Kotlin আপনাকে সব ম্যাচগুলির উপর ইটারেট করার সুযোগ দেয়:

```kotlin
val datePattern = "\\d{2}/\\d{2}/\\d{4}".toRegex()
val input = "Today's date is 07/09/2023."
val dates = datePattern.findAll(input)

for (date in dates) {
    println(date.value)
}
// আউটপুট: 07/09/2023
```

### টেক্সট প্রতিস্থাপন
একটি স্ট্রিং-এর যে অংশগুলো একটি প্যাটার্নের সাথে মেলে সেগুলো প্রতিস্থাপন করা `replace` ফাংশন দিয়ে সোজা:

```kotlin
val input = "Username: user123"
val sanitizedInput = input.replace("\\d+".toRegex(), "XXX")

println(sanitizedInput)  // আউটপুট: Username: userXXX
```

### স্ট্রিং বিভাজন
একটি স্ট্রিংকে একটি regex প্যাটার্ন ব্যবহার করে তালিকায় বিভাজন করুন:

```kotlin
val input = "1,2,3,4,5"
val numbers = input.split(",".toRegex())

println(numbers)  // আউটপুট: [1, 2, 3, 4, 5]
```

### থার্ড-পার্টি লাইব্রেরি: Kotest
[Kotest](https://github.com/kotest/kotest) হল একটি জনপ্রিয় Kotlin টেস্টিং লাইব্রেরি যা Kotlin-এর বিল্ট-ইন regex সাপোর্ট বর্ধিত করে, বিশেষ করে টেস্ট কেসগুলিতে ভ্যালিডেশনের জন্য উপযোগী।

```kotlin
// ধরে নেওয়া হল যে Kotest আপনার প্রজেক্টে যোগ করা আছে
import io.kotest.matchers.string.shouldMatch

val input = "kotlin@test.com"
input shouldMatch "\\S+@\\S+\\.com".toRegex()

// যদি ইনপুট ইমেইল প্যাটার্নের সাথে মেলে তাহলে এই পরীক্ষা পাস হবে।
```

আপনার Kotlin অ্যাপ্লিকেশনগুলিতে নিয়মিত অভিব্যক্তি অন্তর্ভুক্ত করে, আপনি কার্যকরীভাবে জটিল টেক্সট প্রক্রিয়াকরণ সম্পাদন করতে পারেন। আপনি ব্যবহারকারীর ইনপুট যাচাই করছেন, ডাটা এক্সট্র্যাক্ট করছেন, অথবা স্ট্রিংগুলিকে রূপান্তর করছেন, রেগেক্স প্যাটার্ন একটি দৃঢ় সমাধান অফার করে।

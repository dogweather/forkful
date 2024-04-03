---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:13:38.685482-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u0995\u09CB\u099F\u09B2\u09BF\
  \u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \ \u09A5\u09C7\u0995\u09C7 \u0989\u09AD\u09AF\u09BC \u09A7\u09B0\u09A3\u09C7\u09B0\
  \ \u0989\u09A6\u09CD\u09A7\u09C3\u09A4\u09BF\u099A\u09BF\u09B9\u09CD\u09A8 \u0985\
  \u09AA\u09B8\u09BE\u09B0\u09A3\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0996\
  \u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09B9\u099C \u09AA\u09A6\u09CD\
  \u09A7\u09A4\u09BF \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2."
lastmod: '2024-03-17T18:47:43.981136-06:00'
model: gpt-4-0125-preview
summary: "\u0995\u09CB\u099F\u09B2\u09BF\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09B8\
  \u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u0989\u09AD\u09AF\
  \u09BC \u09A7\u09B0\u09A3\u09C7\u09B0 \u0989\u09A6\u09CD\u09A7\u09C3\u09A4\u09BF\
  \u099A\u09BF\u09B9\u09CD\u09A8 \u0985\u09AA\u09B8\u09BE\u09B0\u09A3\u09C7\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF\
  \ \u09B8\u09B9\u099C \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF \u09A6\u09C7\u0993\u09AF\
  \u09BC\u09BE \u09B9\u09B2."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u0989\
  \u09A6\u09CD\u09A7\u09C3\u09A4\u09BF \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\
  \u09BE"
weight: 9
---

## কিভাবে:
কোটলিনে একটি স্ট্রিং থেকে উভয় ধরণের উদ্ধৃতিচিহ্ন অপসারণের জন্য এখানে একটি সহজ পদ্ধতি দেওয়া হল:

```kotlin
fun removeQuotes(input: String): String {
    return input.replace("\"", "").replace("'", "")
}

fun main() {
    val stringWithQuotes = "Kotlin \"rocks\" it's 'cool'"
    val stringWithoutQuotes = removeQuotes(stringWithQuotes)
    println(stringWithoutQuotes) // আউটপুট: Kotlin rocks its cool
}
```

এবং যদি আপনি কেবল এক ধরনের উদ্ধৃতিচিহ্ন অপসারণ করতে চান, তাহলে অন্য replace কল টি বাদ দিন।

```kotlin
fun removeDoubleQuotes(input: String): String {
    return input.replace("\"", "")
}

fun removeSingleQuotes(input: String): String {
    return input.replace("'", "")
}

fun main() {
    val stringWithQuotes = "Kotlin \"rocks\" it's 'cool'"
    println(removeDoubleQuotes(stringWithQuotes)) // আউটপুট: Kotlin rocks it's 'cool'
    println(removeSingleQuotes(stringWithQuotes)) // আউটপুট: Kotlin "rocks" its cool
}
```

## গভীর ডাইভ
ঐতিহাসিকভাবে, স্ট্রিং এবং চরিত্র নির্গমনের সাথে নিয়োগ করা প্রোগ্রামিং-এর এক মৌলিক অংশ হিসেবে রেখে দিয়েছে, যেহেতু টেক্সট আমাদের ডেটার সাথে ইন্টারফেসিং এর একটি মৌলিক উপায়। সোমসময়, স্ট্রিং-এর মধ্যে উদ্ধৃতিচিহ্নগুলি নির্গমিত করা প্রয়োজন হয়। এটি একটি পূর্ববর্তী ব্যাকস্ল্যাশ দ্বারা নির্দেশিত (উদাঃ, `"She said, \"Hi!\""`). এমন স্ট্রিং প্রক্রিয়া করার সময়, আপনার এস্কেপ চরিত্রগুলি অথবা উদ্ধৃতিচিহ্নগুলি নিজেই অপসারণ করা দরকার পারে যাতে পরিষ্কার বা ব্যবহারযোগ্য টেক্সট পাওয়া যায়।

`replace` পদ্ধতির বিকল্পগুলি অন্তর্ভুক্ত আছে রেজেক্স-ভিত্তিক অপসারণ অথবা ম্যানুয়ালভাবে অক্ষরে অক্ষরে স্ট্রিং পার্স করা। তবে, রেজেক্স সিম্পল অপারেশনের জন্য অতিরিক্ত হতে পারে এবং ম্যানুয়াল পার্সিং নির্মিত স্ট্রিং ফাংশন ব্যবহার করার চেয়ে কম কার্যকর। কোটলিনের `replace` ফাংশন Java'র `String` `replace` পদ্ধতির অন্তর্নিহিত সুবিধা নেয়, যেটি কর্মক্ষমতার জন্য ভালভাবে অপ্টিমাইজড।

বাস্তবায়নের দিক থেকে, উল্লেখ্য যে কোটলিন Java এর সাথে ইন্টারঅপারেবল, তাই বাস্তবে, আপনি যেকোনো স্ট্রিং অপারেশনগুলি করবেন তা Java-তে যতটা কর্মক্ষম হবে। উদ্ধৃতিচিহ্ন অপসারণের সময় প্রান্তিক ক্ষেত্রগুলির, যেমন নেস্টেড উদ্ধৃতিচিহ্নগুলি, সচেতন থাকা প্রয়োজন, যা একটি আরও জটিল পদ্ধতি দাবি করতে পারে, সম্ভবত নিয়মিত অভিব্যক্তি বা একটি পার্সার লাইব্রেরি ব্যবহার করে।

## দেখুন
কোটলিনে স্ট্রিং সম্পর্কিত আরও প্রসঙ্গের জন্য, আপনি অফিসিয়াল ডকুমেন্টেশন দেখতে পারেন:

- [কোটলিনের স্ট্রিং ডকুমেন্টেশন](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)

কোটলিনে নিয়মিত অভিব্যক্তি এবং পার্সিং সম্পর্কে গভীর ডাইভের জন্য:

- [কোটলিন রেজেক্স ডকুমেন্টেশন](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)

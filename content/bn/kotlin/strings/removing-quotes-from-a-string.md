---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:13:38.685482-06:00
description: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \ \u09A5\u09C7\u0995\u09C7 \u0989\u09A6\u09CD\u09A7\u09C3\u09A4\u09BF\u099A\u09BF\
  \u09B9\u09CD\u09A8 \u0985\u09AA\u09B8\u09BE\u09B0\u09A3 \u098F\u09B0 \u0985\u09B0\
  \u09CD\u09A5 \u09B9\u09B2 \u098F\u0995\u0995 (' ') \u09AC\u09BE \u09A6\u09CD\u09AC\
  \u09C8\u09A4 (\" \") \u0989\u09A6\u09CD\u09A7\u09C3\u09A4\u09BF\u099A\u09BF\u09B9\
  \u09CD\u09A8\u09C7\u09B0 \u09AF\u09C7\u0995\u09CB\u09A8\u09CB \u0998\u099F\u09A8\
  \u09BE\u0995\u09C7 \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09BE\u099C\u09C7\u09B0\
  \ \u09A1\u09C7\u099F\u09BE \u09A5\u09C7\u0995\u09C7 \u09AC\u09BE\u09A6 \u09A6\u09C7\
  \u0993\u09AF\u09BC\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09BE\u09B0\u09B0\u09BE\u2026"
lastmod: '2024-03-17T18:47:43.981136-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\
  \u09C7\u0995\u09C7 \u0989\u09A6\u09CD\u09A7\u09C3\u09A4\u09BF\u099A\u09BF\u09B9\u09CD\
  \u09A8 \u0985\u09AA\u09B8\u09BE\u09B0\u09A3 \u098F\u09B0 \u0985\u09B0\u09CD\u09A5\
  \ \u09B9\u09B2 \u098F\u0995\u0995 (' ') \u09AC\u09BE \u09A6\u09CD\u09AC\u09C8\u09A4\
  \ (\" \") \u0989\u09A6\u09CD\u09A7\u09C3\u09A4\u09BF\u099A\u09BF\u09B9\u09CD\u09A8\
  \u09C7\u09B0 \u09AF\u09C7\u0995\u09CB\u09A8\u09CB \u0998\u099F\u09A8\u09BE\u0995\
  \u09C7 \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09BE\u099C\u09C7\u09B0 \u09A1\u09C7\
  \u099F\u09BE \u09A5\u09C7\u0995\u09C7 \u09AC\u09BE\u09A6 \u09A6\u09C7\u0993\u09AF\
  \u09BC\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \u09B0\u09BE\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u0989\
  \u09A6\u09CD\u09A7\u09C3\u09A4\u09BF \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\
  \u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

একটি স্ট্রিং থেকে উদ্ধৃতিচিহ্ন অপসারণ এর অর্থ হল একক (' ') বা দ্বৈত (" ") উদ্ধৃতিচিহ্নের যেকোনো ঘটনাকে আপনার কাজের ডেটা থেকে বাদ দেওয়া। প্রোগ্রামাররা প্রায়শই ডেটা পরিষ্কার করার জন্য, আরও প্রক্রিয়াকরণের জন্য প্রস্তুতি বা যখন উদ্ধৃতিচিহ্নগুলি নিজেই ডেটার অর্থের প্রাসঙ্গিক নয় তখন এটি করতে হয়।

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

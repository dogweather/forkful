---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:36.057889-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Kotlin-\u098F\u09B0 `toLowerCase()`\
  \ \u09AB\u09BE\u0982\u09B6\u09A8 \u09A6\u09CD\u09B0\u09C1\u09A4 \u098F\u0995\u099F\
  \u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7 \u09B2\u09CB\u09AF\
  \u09BC\u09BE\u09B0 \u0995\u09C7\u09B8\u09C7 \u09AA\u09B0\u09BF\u09A3\u09A4 \u0995\
  \u09B0\u09C7\u0964 \u098F\u099F\u09BF \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09AC\u09C7\u09A8 \u09A4\u09BE\
  \ \u09A8\u09BF\u099A\u09C7 \u09A6\u09C7\u0996\u09BE\u09A8\u09CB \u09B9\u09B2\u09CB\
  ."
lastmod: '2024-03-17T18:47:43.980179-06:00'
model: gpt-4-0125-preview
summary: "Kotlin-\u098F\u09B0 `toLowerCase()` \u09AB\u09BE\u0982\u09B6\u09A8 \u09A6\
  \u09CD\u09B0\u09C1\u09A4 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982\u0995\u09C7 \u09B2\u09CB\u09AF\u09BC\u09BE\u09B0 \u0995\u09C7\u09B8\
  \u09C7 \u09AA\u09B0\u09BF\u09A3\u09A4 \u0995\u09B0\u09C7\u0964 \u098F\u099F\u09BF\
  \ \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09AC\u09C7\u09A8 \u09A4\u09BE \u09A8\u09BF\u099A\u09C7 \u09A6\u09C7\
  \u0996\u09BE\u09A8\u09CB \u09B9\u09B2\u09CB."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7 \u09B2\u09CB\u09AF\u09BC\
  \u09BE\u09B0 \u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\
  \u09B0 \u0995\u09B0\u09BE"
weight: 4
---

## কিভাবে:
Kotlin-এর `toLowerCase()` ফাংশন দ্রুত একটি স্ট্রিংকে লোয়ার কেসে পরিণত করে। এটি কিভাবে ব্যবহার করবেন তা নিচে দেখানো হলো:

```kotlin
fun main() {
    val originalString = "ThiS iS A MixED cAsE String!"
    val lowerCaseString = originalString.lowercase()

    println(lowerCaseString) // আউটপুট: this is a mixed case string!
}
```
কেবল `lowercase()` কল করুন এবং হয়ে গেল। ইনপুটের ক্যাপ্স ম্যাটার করে না; আউটপুট সবসময় লোয়ার কেসে হবে।

## গভীরে ডুব
Kotlin স্ট্রিংকে লোয়ার-কেসিং এর জন্য নতুন কিছু আবিষ্কার করেনি। এটি আসলে প্রোগ্রামিং ভাষাগুলোতে একটি সাধারণ বৈশিষ্ট্য। ঐতিহাসিকভাবে, C ভাষার `tolower()` ফাংশনের মত ফাংশন দীর্ঘকাল ধরে কেস রূপান্তরে নিযুক্ত।

এখন, লোয়ারকেসিং করার সময় দুটি বিষয়ে মনোযোগ দিতে হবে: লোকেল এবং পারফরম্যান্স। Kotlin-এর `lowercase()` ফাংশনটি একটি `Locale` গ্রহণ করতে পারে কারণ, আশ্চর্য, অক্ষরের কেসিং সর্বজনীন নয়। উদাহরণস্বরূপ, তুর্কি ভাষার দাগযুক্ত এবং দাগহীন 'I' কেস রূপান্তরে অনন্যভাবে আচরণ করে।

পারফরম্যান্স? বেশিরভাগ অ্যাপে, আপনি লক্ষ্য করবেন না। কিন্তু বড় মাপের টেক্সট প্রক্রিয়াকরণে আরো বেশি মেমোরি ও সময় খরচ হয় কারণ Kotlin-এ স্ট্রিংগুলি অপরিবর্তনীয়। যখন আপনি একটি স্ট্রিংকে লোয়ার কেসে পরিণত করেন, আপনি একটি নতুন স্ট্রিং পান। 

পুরনো-স্কুলের মানুষজন `.toLowerCase()` মনে রাখুন — Kotlin এখন স্পষ্টতা জন্য `lowercase()` প্রাধান্য দেয়।

## আরও দেখুন
- Kotlin স্ট্রিং ডকুমেন্টেশন: [Kotlinlang.org](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/lowercase.html)
- টেক্সট প্রসেসিং এবং উন্নত কেস ম্যানিপুলেশনের জন্য, `java.lang.String` API দেখুন: [Oracle ডকস](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
- লোকেল এবং ভাষার বৈচিত্র্য বুঝার জন্য: [Oracle Locale ডকস](https://docs.oracle.com/javase/tutorial/i18n/locale/)

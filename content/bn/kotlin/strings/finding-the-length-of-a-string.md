---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:19.828518-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u0995\u09AE\u09CD\u09AA\u09BF\
  \u0989\u099F\u09BF\u0982 \u098F\u09B0 \u09AA\u09CD\u09B0\u09BE\u09A5\u09AE\u09BF\
  \u0995 \u09A6\u09BF\u09A8\u0997\u09C1\u09B2\u09BF\u09A4\u09C7, C \u09AD\u09BE\u09B7\
  \u09BE\u09DF \u09AE\u09A4\u09CB \u09A8\u09BE\u09B2-\u099F\u09BE\u09B0\u09CD\u09AE\
  \u09BF\u09A8\u09C7\u099F\u09C7\u09A1 \u0985\u09CD\u09AF\u09BE\u09B0\u09C7\u0997\u09C1\
  \u09B2\u09BF \u09A6\u09CD\u09AC\u09BE\u09B0\u09BE \u09B8\u09BE\u09A7\u09BE\u09B0\
  \u09A3\u09A4 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0997\u09C1\u09B2\u09BF\
  \ \u09AD\u09BF\u09A8\u09CD\u09A8\u09AD\u09BE\u09AC\u09C7 \u09B8\u09AE\u09CD\u09AA\
  \u09BE\u09A6\u09BF\u09A4 \u09B9\u09A4\u0964 \u0986\u09A7\u09C1\u09A8\u09BF\u0995\
  \ \u09AD\u09BE\u09B7\u09BE \u09B9\u09BF\u09B8\u09BE\u09AC\u09C7,\u2026"
lastmod: '2024-04-05T22:40:38.125669-06:00'
model: gpt-4-0125-preview
summary: "\u0995\u09AE\u09CD\u09AA\u09BF\u0989\u099F\u09BF\u0982 \u098F\u09B0 \u09AA\
  \u09CD\u09B0\u09BE\u09A5\u09AE\u09BF\u0995 \u09A6\u09BF\u09A8\u0997\u09C1\u09B2\u09BF\
  \u09A4\u09C7, C \u09AD\u09BE\u09B7\u09BE\u09DF \u09AE\u09A4\u09CB \u09A8\u09BE\u09B2\
  -\u099F\u09BE\u09B0\u09CD\u09AE\u09BF\u09A8\u09C7\u099F\u09C7\u09A1 \u0985\u09CD\
  \u09AF\u09BE\u09B0\u09C7\u0997\u09C1\u09B2\u09BF \u09A6\u09CD\u09AC\u09BE\u09B0\u09BE\
  \ \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\u09A4 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982\u0997\u09C1\u09B2\u09BF \u09AD\u09BF\u09A8\u09CD\u09A8\u09AD\u09BE\u09AC\u09C7\
  \ \u09B8\u09AE\u09CD\u09AA\u09BE\u09A6\u09BF\u09A4 \u09B9\u09A4\u0964 \u0986\u09A7\
  \u09C1\u09A8\u09BF\u0995 \u09AD\u09BE\u09B7\u09BE \u09B9\u09BF\u09B8\u09BE\u09AC\
  \u09C7, \u0995\u09CB\u099F\u09B2\u09BF\u09A8 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u0985\u09AC\u099C\u09C7\u0995\u09CD\u099F\u09C7\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 `length` \u09AA\u09CD\u09B0\u09AA\
  \u09BE\u09B0\u09CD\u099F\u09BF \u09B8\u09B0\u09AC\u09B0\u09BE\u09B9 \u0995\u09B0\
  \u09C7\u0964 \u09AC\u09BF\u0995\u09B2\u09CD\u09AA?"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09A6\u09C8\u09B0\
  \u09CD\u0998\u09CD\u09AF \u099A\u09BF\u09B9\u09CD\u09A8\u09BF\u09A4 \u0995\u09B0\
  \u09BE"
weight: 7
---

## কিভাবে:
```kotlin
fun main() {
    val greeting = "Hello, World!"
    println(greeting.length)  // 13 প্রিন্ট করে
}
```
আউটপুট:
```
13
```

## গভীরে যাচাই
কম্পিউটিং এর প্রাথমিক দিনগুলিতে, C ভাষায় মতো নাল-টার্মিনেটেড অ্যারেগুলি দ্বারা সাধারণত স্ট্রিংগুলি ভিন্নভাবে সম্পাদিত হত। আধুনিক ভাষা হিসাবে, কোটলিন স্ট্রিং অবজেক্টের জন্য বিল্ট-ইন `length` প্রপার্টি সরবরাহ করে।

বিকল্প? ভাল, আপনি একটি স্ট্রিং এর মাধ্যমে লুপ করে অক্ষরগুলি গণনা করতে পারেন—কিন্তু কেন চাকা পুনরায় আবিষ্কার করবেন? কোটলিনের `length` দক্ষ এবং সহজ।

ভিতরের অংশে, `length` স্ট্রিংটিতে UTF-16 কোড ইউনিটগুলির সংখ্যা ফেরত দেয়। এর মানে হল যে বেশিরভাগ টেক্সটের (যেমন ইংরেজি) জন্য, কোড ইউনিটগুলির সংখ্যা এবং অক্ষরগুলির সংখ্যা মিলে যায়। তবে, মৌলিক বহুভাষিক পরিক্ষেত্রের (BMP) বাইরের অক্ষরগুলির জন্য, যেগুলি দুটি কোড ইউনিট দ্বারা প্রতিনিধিত্ব করা হয় (একটি সারোগেট জোড়া), `length` প্রপার্টি ইউনিকোড কোড পয়েন্টের সংখ্যার সাথে মিলতে পারে না।

## দেখাও
- স্ট্রিংস এর জন্য কোটলিন স্ট্যান্ডার্ড লাইব্রেরি রেফারেন্স: [কোটলিন স্ট্রিংস](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- UTF-16 এবং অক্ষর প্রতিনিধিত্ব বুঝতে: [জাভায় ইউনিকোড](https://docs.oracle.com/javase/tutorial/i18n/text/unicode.html)
- স্ট্রিংগুলি এবং সম্পর্কিত ফাংশনগুলির কোটলিনের হ্যান্ডলিং নিয়ে গভীর বিশ্লেষণ: [জাভা ডেভেলপারদের জন্য কোটলিন](https://www.coursera.org/learn/kotlin-for-java-developers)

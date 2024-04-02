---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:50:49.413481-06:00
description: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\
  \u09B0\u09AA\u09CB\u09B2\u09C7\u09B6\u09A8 \u0986\u09AA\u09A8\u09BE\u0995\u09C7\
  \ \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \u0997\u09C1\u09B2\u09BF\u09A4\u09C7 \u09AD\u09C7\u09B0\u09BF\u09AF\u09BC\u09C7\u09AC\
  \u09B2\u0997\u09C1\u09B2\u09BF \u098F\u09AE\u09CD\u09AC\u09C7\u09A1 \u0995\u09B0\
  \u09A4\u09C7 \u09A6\u09C7\u09AF\u09BC\u0964 \u098F\u099F\u09BF \u099C\u099F\u09BF\
  \u09B2 \u09AF\u09CB\u0997\u09AC\u09BF\u09A8\u09CD\u09AF\u09BE\u09B8 \u099B\u09BE\
  \u09A1\u09BC\u09BE\u0987 \u09A1\u09BE\u09AF\u09BC\u09A8\u09BE\u09AE\u09BF\u0995\
  , \u09AA\u09A0\u09A8\u09AF\u09CB\u0997\u09CD\u09AF \u099F\u09C7\u0995\u09CD\u09B8\
  \u099F \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF\
  \u2026"
lastmod: '2024-03-17T18:47:43.978972-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\
  \u09B0\u09AA\u09CB\u09B2\u09C7\u09B6\u09A8 \u0986\u09AA\u09A8\u09BE\u0995\u09C7\
  \ \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \u0997\u09C1\u09B2\u09BF\u09A4\u09C7 \u09AD\u09C7\u09B0\u09BF\u09AF\u09BC\u09C7\u09AC\
  \u09B2\u0997\u09C1\u09B2\u09BF \u098F\u09AE\u09CD\u09AC\u09C7\u09A1 \u0995\u09B0\
  \u09A4\u09C7 \u09A6\u09C7\u09AF\u09BC\u0964 \u098F\u099F\u09BF \u099C\u099F\u09BF\
  \u09B2 \u09AF\u09CB\u0997\u09AC\u09BF\u09A8\u09CD\u09AF\u09BE\u09B8 \u099B\u09BE\
  \u09A1\u09BC\u09BE\u0987 \u09A1\u09BE\u09AF\u09BC\u09A8\u09BE\u09AE\u09BF\u0995\
  , \u09AA\u09A0\u09A8\u09AF\u09CB\u0997\u09CD\u09AF \u099F\u09C7\u0995\u09CD\u09B8\
  \u099F \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF\
  \u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\
  \u09AA\u09CB\u09B2\u09C7\u099F \u0995\u09B0\u09BE"
weight: 8
---

## কি এবং কেন?

স্ট্রিং ইন্টারপোলেশন আপনাকে সরাসরি স্ট্রিংগুলিতে ভেরিয়েবলগুলি এম্বেড করতে দেয়। এটি জটিল যোগবিন্যাস ছাড়াই ডায়নামিক, পঠনযোগ্য টেক্সট তৈরি করার জন্য সুবিধাজনক।

## কিভাবে:
```kotlin
fun main() {
    val name = "Alex"
    val age = 29
    // স্ট্রিংয়ে ভেরিয়েবলসমূহ ইন্টারপোলেট করুন
    val greeting = "হ্যালো, আমার নাম $name এবং আমি $age বছর বয়সী।"
    println(greeting) // আউটপুট: হ্যালো, আমার নাম Alex এবং আমি 29 বছর বয়সী।

    // স্ট্রিংগুলিতে এক্সপ্রেশন
    val announcement = "পরের বছর, আমি হব ${age + 1}!"
    println(announcement) // আউটপুট: পরের বছর, আমি হব 30!
}
```

## গভীর ডুব
Kotlin অন্যান্য আধুনিক ভাষার প্রভাবে, জাভার স্ট্রিং যোগবিন্যাসের তুলনায় একটি পরিষ্কার বিকল্প হিসেবে স্ট্রিং ইন্টারপোলেশন চালু করে। এটি পাঠ্যগততা বৃদ্ধি করে এবং কোড সরলীকরণ করে।

ইতিহাসে, জাভা বিরাট `+` ব্যবহার করে যোগবিন্যাস চালাতে বাধ্য করত, যা পড়তে কঠিন এবং কম দক্ষ হতে পারে, কারণ এটি একাধিক স্ট্রিং অবজেক্ট তৈরি করত। Kotlin’র পদ্ধতি আরও শক্তিশালী, কেবল ভেরিয়েবল এম্বেড না করা সাথে সাথে স্ট্রিংগুলিতে এক্সপ্রেশন মূল্যায়ন করা সম্ভব করে।

ব্যাকগ্রাউন্ডে, Kotlin এই ইন্টারপোলেশনটি `StringBuilder` অপারেশনগুলিতে বা জটিলতার উপর নির্ভর করে স্ট্রিং যোগবিন্যাসে পরিণত করে, ডেভেলপারের উপরে থেকে বোঝা নামায়। 

স্ট্রিং ইন্টারপোলেশনের বিকল্পগুলি ব্যাপক টেক্সট ম্যানিপুলেশনের জন্য টেমপ্লেট ইঞ্জিনসহ রয়েছে, কিন্তু কোডে, ডায়নামিক কন্টেন্ট অন্তর্ভুক্ত করার জন্য ইন্টারপোলেশন সাধারণত সবচেয়ে দ্রুত উপায় হয়ে উঠেছে।

## আরও দেখুন
- [স্ট্রিং টেমপ্লেটস সম্পর্কে Kotlin ডকুমেন্টেশন](https://kotlinlang.org/docs/basic-syntax.html#string-templates)
- [Kotlin এর `String` API](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [জাভা এবং Kotlin স্ট্রিং যোগবিন্যাস পারফরমেন্স তুলনা](https://proandroiddev.com/the-cost-of-kotlin-language-features-8f7035e9dcb9)

---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:42.162825-06:00
description: "\u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982\u09AF\
  \u09BC\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \ \u0995\u09C7 \u09AC\u09A1\u09BC \u09B9\u09BE\u09A4\u09C7\u09B0 \u0985\u0995\u09CD\
  \u09B7\u09B0\u09C7 \u09AA\u09B0\u09BF\u09A3\u09A4 \u0995\u09B0\u09BE\u09B0 \u09AA\
  \u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE \u09AE\u09C2\u09B2\u09A4\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u099F\u09BF\u09B0 \u09AA\u09CD\u09B0\
  \u09A5\u09AE \u0985\u0995\u09CD\u09B7\u09B0\u0995\u09C7 \u0989\u09AA\u09B0\u09C7\
  \u09B0 \u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0\
  \ \u0995\u09B0\u09BE \u09AF\u09A6\u09BF \u09A4\u09BE \u0987\u09A4\u09BF\u09AE\u09A7\
  \u09CD\u09AF\u09C7 \u09A8\u09BE \u09B9\u09AF\u09BC, \u09AF\u09BE\u2026"
lastmod: '2024-03-17T18:47:43.975710-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982\u09AF\
  \u09BC\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \ \u0995\u09C7 \u09AC\u09A1\u09BC \u09B9\u09BE\u09A4\u09C7\u09B0 \u0985\u0995\u09CD\
  \u09B7\u09B0\u09C7 \u09AA\u09B0\u09BF\u09A3\u09A4 \u0995\u09B0\u09BE\u09B0 \u09AA\
  \u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE \u09AE\u09C2\u09B2\u09A4\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u099F\u09BF\u09B0 \u09AA\u09CD\u09B0\
  \u09A5\u09AE \u0985\u0995\u09CD\u09B7\u09B0\u0995\u09C7 \u0989\u09AA\u09B0\u09C7\
  \u09B0 \u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0\
  \ \u0995\u09B0\u09BE \u09AF\u09A6\u09BF \u09A4\u09BE \u0987\u09A4\u09BF\u09AE\u09A7\
  \u09CD\u09AF\u09C7 \u09A8\u09BE \u09B9\u09AF\u09BC, \u09AF\u09BE\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09AA\u09CD\u09B0\
  \u09A5\u09AE \u0985\u0995\u09CD\u09B7\u09B0 \u09AC\u09A1\u09BC \u09B9\u09BE\u09A4\
  \u09C7\u09B0 \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

প্রোগ্রামিংয়ে একটি স্ট্রিং কে বড় হাতের অক্ষরে পরিণত করার প্রক্রিয়া মূলত স্ট্রিংটির প্রথম অক্ষরকে উপরের কেসে রূপান্তর করা যদি তা ইতিমধ্যে না হয়, যা ব্যবহারকারীর ইনপুট সমন্বয় বা ইউজার ইন্টারফেসে পাঠ্য আরেকটু মানোন্নয়ন বা মানব-বান্ধব উপায়ে প্রদর্শনের জন্য উপযোগী। প্রোগ্রামাররা তাদের সফ্টওয়্যার অ্যাপ্লিকেশনের ভেতরে নির্দিষ্ট ফর্ম্যাটিং দাবিগুলি পূরণ বা ডেটা সামঞ্জস্য নিশ্চিত করার জন্য এই অপারেশন সম্পাদন করে।

## কীভাবে:

Kotlin-এ, স্ট্যান্ডার্ড লাইব্রেরি ফাংশন ব্যবহার করে বিনা থার্ড-পার্টি লাইব্রেরির প্রয়োজনে স্ট্রিংগুলিকে বড় হাতের অক্ষরে পরিণত করা যায়। Kotlin-এর স্ট্রিংস হ্যান্ডলিং পদ্ধতি এই অপারেশনগুলিকে সোজা এবং সংক্ষিপ্ত করে তোলে।

### সম্পূর্ণ স্ট্রিংটি বড় হাতের অক্ষরে পরিণত করা:

```kotlin
val message = "hello, world!"
val capitalizedMessage = message.uppercase()

println(capitalizedMessage) // আউটপুট: HELLO, WORLD!
```

### শুধুমাত্র প্রথম অক্ষরটি বড় হাতের অক্ষরে পরিণত করা:

Kotlin 1.5 এর মতে, `capitalize()` ফাংশন বাতিল করা হয়েছে এবং তা `replaceFirstChar` এবং একটি লাম্বডা দ্বারা প্রতিস্থাপিত করা হয়েছে যা যাচাই করে যে এটি যদি ছোট হাতের অক্ষর হয় তাহলে তা উপরের কেসে রূপান্তরিত করে।

```kotlin
val greeting = "hello, world!"
val capitalizedGreeting = greeting.replaceFirstChar {
    if (it.isLowerCase()) it.titlecase() else it.toString()
}

println(capitalizedGreeting) // আউটপুট: Hello, world!
```

এই প্রক্রিয়াটি বাকি বাক্যটির আসল রূপের সাথে শুধুমাত্র প্রথম অক্ষরটিকে উপরের কেসে পরিণত করে।

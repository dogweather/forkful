---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:42.162825-06:00
description: "\u0995\u09C0\u09AD\u09BE\u09AC\u09C7: Kotlin-\u098F, \u09B8\u09CD\u099F\
  \u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u09B2\u09BE\u0987\u09AC\
  \u09CD\u09B0\u09C7\u09B0\u09BF \u09AB\u09BE\u0982\u09B6\u09A8 \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09AC\u09BF\u09A8\u09BE \u09A5\u09BE\
  \u09B0\u09CD\u09A1-\u09AA\u09BE\u09B0\u09CD\u099F\u09BF \u09B2\u09BE\u0987\u09AC\
  \u09CD\u09B0\u09C7\u09B0\u09BF\u09B0 \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8\
  \u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0997\u09C1\u09B2\u09BF\u0995\u09C7\
  \ \u09AC\u09A1\u09BC \u09B9\u09BE\u09A4\u09C7\u09B0 \u0985\u0995\u09CD\u09B7\u09B0\
  \u09C7 \u09AA\u09B0\u09BF\u09A3\u09A4 \u0995\u09B0\u09BE \u09AF\u09BE\u09AF\u09BC\
  \u0964 Kotlin-\u2026"
lastmod: '2024-03-17T18:47:43.975710-06:00'
model: gpt-4-0125-preview
summary: "Kotlin-\u098F, \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\
  \u09B0\u09CD\u09A1 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09AB\
  \u09BE\u0982\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09C7 \u09AC\u09BF\u09A8\u09BE \u09A5\u09BE\u09B0\u09CD\u09A1-\u09AA\u09BE\u09B0\
  \u09CD\u099F\u09BF \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u09B0\
  \ \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8\u09C7 \u09B8\u09CD\u099F\u09CD\
  \u09B0\u09BF\u0982\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09AC\u09A1\u09BC \u09B9\
  \u09BE\u09A4\u09C7\u09B0 \u0985\u0995\u09CD\u09B7\u09B0\u09C7 \u09AA\u09B0\u09BF\
  \u09A3\u09A4 \u0995\u09B0\u09BE \u09AF\u09BE\u09AF\u09BC\u0964 Kotlin-\u098F\u09B0\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09B8 \u09B9\u09CD\u09AF\u09BE\u09A8\
  \u09CD\u09A1\u09B2\u09BF\u0982 \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF \u098F\u0987\
  \ \u0985\u09AA\u09BE\u09B0\u09C7\u09B6\u09A8\u0997\u09C1\u09B2\u09BF\u0995\u09C7\
  \ \u09B8\u09CB\u099C\u09BE \u098F\u09AC\u0982 \u09B8\u0982\u0995\u09CD\u09B7\u09BF\
  \u09AA\u09CD\u09A4 \u0995\u09B0\u09C7 \u09A4\u09CB\u09B2\u09C7\u0964\n\n#."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09AA\u09CD\u09B0\
  \u09A5\u09AE \u0985\u0995\u09CD\u09B7\u09B0 \u09AC\u09A1\u09BC \u09B9\u09BE\u09A4\
  \u09C7\u09B0 \u0995\u09B0\u09BE"
weight: 2
---

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

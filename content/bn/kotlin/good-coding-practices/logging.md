---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:55:41.593394-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u0995\u099F\u09B2\u09BF\u09A8\
  \u09C7, \u09B2\u0997\u09BF\u0982 \u0995\u09B0\u09BE \u09AF\u09C7\u09A4\u09C7 \u09AA\
  \u09BE\u09B0\u09C7 \u09B8\u09B9\u099C \u0995\u09CD\u09B7\u09C7\u09A4\u09CD\u09B0\
  \u0997\u09C1\u09B2\u09BF\u09A4\u09C7 \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8\
  \ `println()` \u09AB\u09BE\u0982\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09C7, \u0985\u09A5\u09AC\u09BE \u0985\u09A7\u09BF\u0995 \u099C\
  \u099F\u09BF\u09B2 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09C0 \u09AF\
  \u09C7\u09AE\u09A8 SLF4J \u09B8\u09BE\u09A5\u09C7 Logback \u0985\u09A5\u09AC\u09BE\
  \ Log4j\u2026"
lastmod: '2024-04-05T21:53:52.325373-06:00'
model: gpt-4-0125-preview
summary: "\u0995\u099F\u09B2\u09BF\u09A8\u09C7, \u09B2\u0997\u09BF\u0982 \u0995\u09B0\
  \u09BE \u09AF\u09C7\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7 \u09B8\u09B9\u099C \u0995\
  \u09CD\u09B7\u09C7\u09A4\u09CD\u09B0\u0997\u09C1\u09B2\u09BF\u09A4\u09C7 \u09AC\u09BF\
  \u09B2\u09CD\u099F-\u0987\u09A8 `println()` \u09AB\u09BE\u0982\u09B6\u09A8 \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7, \u0985\u09A5\u09AC\u09BE\
  \ \u0985\u09A7\u09BF\u0995 \u099C\u099F\u09BF\u09B2 \u09B2\u09BE\u0987\u09AC\u09CD\
  \u09B0\u09C7\u09B0\u09C0 \u09AF\u09C7\u09AE\u09A8 SLF4J \u09B8\u09BE\u09A5\u09C7\
  \ Logback \u0985\u09A5\u09AC\u09BE Log4j \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09C7 \u0989\u09A8\u09CD\u09A8\u09A4 \u09AA\u09CD\u09B0\u09AF\u09BC\
  \u09CB\u099C\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\u0964 \u09A8\u09C0\u099A\
  \u09C7 `println()` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \ \u098F\u0995\u099F\u09BF \u09AE\u09CC\u09B2\u09BF\u0995 \u0989\u09A6\u09BE\u09B9\
  \u09B0\u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u0986\u099B\u09C7."
title: "\u09B2\u0997\u09BF\u0982"
weight: 17
---

## কিভাবে:
কটলিনে, লগিং করা যেতে পারে সহজ ক্ষেত্রগুলিতে বিল্ট-ইন `println()` ফাংশন ব্যবহার করে, অথবা অধিক জটিল লাইব্রেরী যেমন SLF4J সাথে Logback অথবা Log4j ব্যবহার করে উন্নত প্রয়োজনের জন্য।

নীচে `println()` ব্যবহার করে একটি মৌলিক উদাহরণ দেওয়া আছে:

```Kotlin
fun main() {
    println("Simple log message: Application started.")
    // ... এখানে কিছু অ্যাপ্লিকেশন যুক্তি ...
    try {
        // একটি ত্রুটি অনুকরণ করা
        throw Exception("Simulated error")
    } catch (e: Exception) {
        println("Error log message: " + e.message)
    }
}
```

আউটপুট:
```
Simple log message: Application started.
Error log message: Simulated error
```

এবং এখানে SLF4J সাথে Logback কনফিগার করা ব্যবহার করে একটি উদাহরণ দেওয়া আছে:

```Kotlin
import org.slf4j.LoggerFactory

private val logger = LoggerFactory.getLogger("MyAppLogger")

fun main() {
    logger.info("Structured log message: App launched.")
    // ... এখানে কিছু অ্যাপ্লিকেশন যুক্তি ...
    try {
        // একটি ত্রুটি অনুকরণ করা
        throw Exception("Simulated error")
    } catch (e: Exception) {
        logger.error("Structured error log: ", e)
    }
}
```

যথাযথ Logback কনফিগারেশনের ধরে নিয়ে, আউটপুটটি সাজানো হতে পারে এবং এটি লগ ফাইলে লেখা হলে দেখতে এমন হতে পারে:
```
[INFO] - 2023-03-29 14:15:42 - MyAppLogger - Structured log message: App launched.
[ERROR] - 2023-03-29 14:15:43 - MyAppLogger - Structured error log: 
java.lang.Exception: Simulated error
   at com.myapp.Main.main(Main.kt:10)
```

## গভীর ডুব
ঐতিহাসিকভাবে, সফটওয়্যারে লগিং অ্যাপ্লিকেশন এবং সিস্টেমের বাড়তে থাকা জটিলতার সাথে বিকশিত হয়েছে। প্রাথমিক দিনগুলিতে সহজ প্রিন্ট বিবৃতিগুলি যথেষ্ট ছিল, যেখানে প্রোগ্রামগুলি প্রায়ই ডেভেলপার নিজেরা চালানো এবং ডিবাগ করা হত। কিন্তু যেমন সিস্টেমগুলি নেটওয়ার্কড এবং ভিন্ন ব্যবহারকারীদের মধ্যে বিভিন্ন পরিবেশে চালানো হত, একটি দৃঢ় এবং স্থায়ী লগিং সিস্টেম অপরিহার্য হয়ে পড়ত।

কটলিন জনপ্রিয় হওয়ার আগে, জাভা ডেভেলপাররা প্রশস্তভাবে Log4j এবং পরবর্তীতে SLF4J মত লাইব্রেরীগুলি গ্রহণ করেছিলেন। এই প্রথাগুলি কটলিনে অনুপ্রেরণা জাগিয়েছে, জাভা লাইব্রেরীগুলির সাথে কটলিনের ইন্টারঅপারেবিলিটি ব্যবহার করে। SLF4J একটি অ্যাবস্ট্র্যাকশন স্তর হিসাবে কাজ করে, যা বাস্তব লগিং বাস্তবায়নটি পরিবর্তন করার অনুমতি দেয়—সাধারনত Logback বা Log4j2 পছন্দের পছন্দ হয়।

কটলিন জেভিএম, জাভাস্ক্রিপ্ট, এবং নেটিভের মধ্যে কাজ করা মাল্টি-প্ল্যাটফর্ম লগিং সমাধানগুলির জন্যও অনুমতি দেয়, যেমন `expect`/`actual` মেকানিজমের মাধ্যমে, যা প্ল্যাটফর্ম-নির্দিষ্ট বাস্তবায়নগুলিকে অব্যাহতি দেয়।

উৎসর্গীকৃত লগিং লাইব্রেরীগুলির বিপরীতে, println সহজতম লগিং ফর্ম হিসেবে টিকে আছে কারণ এটি অতিরিক্ত সেটআপ বা নির্ভরতা প্রয়োজন করে না; তবে, এটি সাধারণত উৎপাদন অ্যাপ্লিকেশনের জন্য অনুপযুক্ত হয় এর লগ লেভেল, লগ রোটেশন, এবং স্ট্রাকচারড ফরম্যাটের মতো বৈশিষ্ট্যের অভাবের কারণে।

উন্নত লগিং ফ্রেমওয়ার্কের অন্যান্য সাধারণ বৈশিষ্ট্যগুলি অন্তর্ভুক্ত:

- লগ মেসেজের জরুরিতার বিভাগকরণের জন্য লগ লেভেল (DEBUG, INFO, WARN, ERROR, ইত্যাদি)।
- বিভিন্ন সিংকে আউটপুট, যেমন কনসোল, ফাইল, ডাটাবেস, বা নেটওয়ার্ক সেবা।
- স্বয়ংক্রিয় লগ রোটেশন এবং রিটেনশন নীতি।
- মাইক্রোসার্ভিস আর্কিটেকচারের জন্য বিতরণিত ট্রেসিং সাপোর্ট।
- JSON এর মতো ফরম্যাট ব্যবহার করে স্ট্রাকচারড লগিং, যা লগ অ্যানালিটিক্স সিস্টেমের সাথে ভালভাবে মিলিত হয়।

এই টুলগুলি এবং বৈশিষ্ট্যগুলি জটিল, বিতরণিত, অথবা উচ্চ স্কেলের পরিবেশে একটি নির্ভরযোগ্য, পর্যবেক্ষণযোগ্য সিস্টেম বজায় রাখার জন্য অপরিহার্য।

## আরও দেখুন
কটলিন লগিং সম্পর্কে আরও শিক্ষা এবং অন্তর্দৃষ্টির জন্য দেখুন:

- SLF4J (জাভার জন্য সিম্পল লগিং ফ্যাসেড) [http://www.slf4j.org/](http://www.slf4j.org/)
- Logback, Log4j এর উত্তরাধিকার [http://logback.qos.ch/](http://logback.qos.ch/)
- Log4j 2 [https://logging.apache.org/log4j/2.x/](https://logging.apache.org/log4j/2.x/)
- 'expect' এবং 'actual' ঘোষণাসমূহের উপর কটলিন মাল্টি-প্ল্যাটফর্ম ডকুমেন্টেশন: [https://kotlinlang.org/docs/multiplatform.html](https://kotlinlang.org/docs/multiplatform.html)
- কটলিনে স্ট্রাকচারড লগিং গাইড: [https://ktor.io/docs/logging.html](https://ktor.io/docs/logging.html)

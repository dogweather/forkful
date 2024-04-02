---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:44.030147-06:00
description: "\u09A4\u09BE\u09B0\u09BF\u0996\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982\u09DF\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\
  \u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09A4\u09BE\u09B0\
  \u09BF\u0996 \u0985\u09AC\u099C\u09C7\u0995\u09CD\u099F\u0995\u09C7 \u09AE\u09BE\
  \u09A8\u09C1\u09B7\u09C7\u09B0 \u09AC\u09CB\u09A7\u0997\u09AE\u09CD\u09AF \u099F\
  \u09C7\u0995\u09CD\u09B8\u099F\u09C7 \u09AA\u09B0\u09BF\u09A3\u09A4 \u0995\u09B0\
  \u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\
  \u09BE \u09A4\u09BE\u09B0\u09BF\u0996\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09AC\
  \u09CB\u099D\u09BE\u09B0 \u0989\u09AA\u09AF\u09C1\u0995\u09CD\u09A4 \u09AB\u09B0\
  \u09AE\u09CD\u09AF\u09BE\u099F\u09C7 \u09AA\u09CD\u09B0\u09A6\u09B0\u09CD\u09B6\u09A8\
  \ \u0995\u09B0\u09A4\u09C7\u2026"
lastmod: '2024-03-17T18:47:43.633324-06:00'
model: gpt-4-0125-preview
summary: "\u09A4\u09BE\u09B0\u09BF\u0996\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982\u09DF\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\
  \u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09A4\u09BE\u09B0\
  \u09BF\u0996 \u0985\u09AC\u099C\u09C7\u0995\u09CD\u099F\u0995\u09C7 \u09AE\u09BE\
  \u09A8\u09C1\u09B7\u09C7\u09B0 \u09AC\u09CB\u09A7\u0997\u09AE\u09CD\u09AF \u099F\
  \u09C7\u0995\u09CD\u09B8\u099F\u09C7 \u09AA\u09B0\u09BF\u09A3\u09A4 \u0995\u09B0\
  \u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\
  \u09BE \u09A4\u09BE\u09B0\u09BF\u0996\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09AC\
  \u09CB\u099D\u09BE\u09B0 \u0989\u09AA\u09AF\u09C1\u0995\u09CD\u09A4 \u09AB\u09B0\
  \u09AE\u09CD\u09AF\u09BE\u099F\u09C7 \u09AA\u09CD\u09B0\u09A6\u09B0\u09CD\u09B6\u09A8\
  \ \u0995\u09B0\u09A4\u09C7\u2026"
title: "\u09A4\u09BE\u09B0\u09BF\u0996\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u098F \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE"
weight: 28
---

## কি এবং কেন?
তারিখকে স্ট্রিংয়ে রূপান্তর করা মানে একটি তারিখ অবজেক্টকে মানুষের বোধগম্য টেক্সটে পরিণত করা। প্রোগ্রামাররা তারিখগুলিকে বোঝার উপযুক্ত ফরম্যাটে প্রদর্শন করতে অথবা সংরক্ষণ ও ট্রান্সমিশনের জন্য সিরিয়ালাইজ করতে এটি করে থাকেন।

## কিভাবে:
Clojureতে, আমরা জাভা ইন্টারপ সুবিধাগুলি ব্যবহার করে তারিখগুলি ফরম্যাট করি। এখানে একটি দ্রুত গাইড রয়েছে:

```clojure
(import java.text.SimpleDateFormat)
(import java.util.Date)

;; একটি তারিখ অবজেক্ট তৈরি করুন (আসুন বর্তমান তারিখ এবং সময় ব্যবহার করি)
(def now (Date.))

;; কাঙ্খিত ফর্ম্যাট সেট আপ করুন
(def formatter (SimpleDateFormat. "yyyy-MM-dd HH:mm:ss"))

;; তারিখটি স্ট্রিং হিসাবে ফর্ম্যাট করুন
(def formatted-date (.format formatter now))

;; এটি প্রিন্ট করুন
(println formatted-date)
;; আউটপুট হবে: "2023-03-15 09:26:45" (বর্তমান তারিখ এবং সময়ের উপর নির্ভর করবে)
```

## গভীরে ডুব দেওয়া
তারিখগুলিকে স্ট্রিংয়ে রূপান্তর করা শুধুমাত্র Clojureতে নয়, অনেক প্রোগ্রামিং ভাষায় একটি সাধারণ অপারেশন। ঐতিহাসিকভাবে, এই প্রয়োজন তখন উঠে আসে যখন কম্পিউটারগুলি তারিখ হ্যান্ডেল করা শুরু করে কারণ মানুষের বোধগম্য উপস্থাপনা বোঝা এবং যোগাযোগে সহায়তা করে, যেখানে মেশিনগুলি আরও গঠিত ডেটা ফরম্যাট পছন্দ করে।

Clojure কারণ এটি জাভা ভার্চুয়াল মেশিন (JVM) এ চলে, আমরা সাধারণত `java.util.Date` এবং `java.text.SimpleDateFormat` এর মতো জাভার তারিখ এবং সময়ের লাইব্রেরিগুলি ব্যবহার করে থাকি। যদিও এই ক্লাসগুলি দীর্ঘস্থায়ী, নতুন `java.time` প্যাকেজ (জাভা 8 এ পরিচিত) আরও ভাল থ্রেড-সেফটি এবং আরও সহজবোধ্য API সহ একটি বিকল্প প্রস্তাব করে।

Clojure এর কোর ভাষা অংশ হিসেবে কোন বিল্ট-ইন তারিখ ফরম্যাটিং লাইব্রেরি নেই, তাই জাভা ইন্টারপ বা তৃতীয় পক্ষের লাইব্রেরিগুলি, যেমন `clj-time` (জোদা টাইমের একটি র‌্যাপার) ব্যবহার করে আরও সহজলভ্য Clojure সমাধানের জন্য স্বাভাবিক।

এখানে `java.time` ফর্ম্যাটিং ব্যবহার করার উপায়টি উপস্থাপিত হল:

```clojure
(import java.time.LocalDateTime)
(import java.time.format.DateTimeFormatter)

;; একটি তারিখ অবজেক্ট তৈরি করুন (বর্তমান তারিখ এবং সময়)
(def now (LocalDateTime/now))

;; কাঙ্খিত ফর্ম্যাট সেট আপ করুন
(def formatter (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss"))

;; তারিখটি স্ট্রিং হিসাবে ফর্ম্যাট করুন
(def formatted-date (.format now formatter))

;; এটি প্রিন্ট করুন
(println formatted-date)
;; আগের মত আউটপুট পাওয়া যাবে, বর্তমান তারিখ এবং সময়ের সাথে
```

এই পদ্ধতি SimpleDateFormat এর সাথে উপস্থিত mutability সমস্যাগুলি এড়িয়ে চলে এবং থ্রেড-সেফটি একটি উদ্বেগ হলে নতুন কোডে প্রাধান্য পেতে হবে।

## দেখুন
- জাভা 8 তারিখ এবং সময় গাইড: [https://docs.oracle.com/javase/tutorial/datetime/](https://docs.oracle.com/javase/tutorial/datetime/)
- ClojureDocs, একটি সম্প্রদায়-প্রেরিত ডকুমেন্টেশন এবং উদাহরণ রেপোজিটরি: [https://clojuredocs.org/](https://clojuredocs.org/)
- clj-time, ক্লোজারের জন্য একটি তারিখ এবং সময় লাইব্রেরি: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)

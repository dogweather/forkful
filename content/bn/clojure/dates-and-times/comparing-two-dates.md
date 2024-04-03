---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:33.473001-06:00
description: "\u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\u09B0\
  \ \u09A4\u09C1\u09B2\u09A8\u09BE \u09AE\u09BE\u09A8\u09C7 \u09A4\u09BE\u09A6\u09C7\
  \u09B0 \u09B8\u09AE\u09CD\u09AA\u09B0\u09CD\u0995 \u0995\u09C0 \u09A4\u09BE \u09AA\
  \u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\u09BE\u2014\u0995\u09CB\u09A8\u099F\
  \u09BF \u0986\u0997\u09C7\u09B0, \u09AA\u09B0\u09C7\u09B0 \u09A8\u09BE\u0995\u09BF\
  \ \u09A0\u09BF\u0995 \u0985\u09AA\u09B0\u099F\u09BF\u09B0 \u09AE\u09A4\u09CB\u0987\
  ? \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u09A1\u09C7\u09A1\u09B2\u09BE\u0987\u09A8 \u09A8\u09BF\u09B0\u09CD\u09AC\u09BE\
  \u09B9, \u0987\u09AD\u09C7\u09A8\u09CD\u099F \u09B8\u09C2\u099A\u09BF\u09AC\u09A6\
  \u09CD\u09A7 \u0995\u09B0\u09BE \u098F\u09AC\u0982\u2026"
lastmod: '2024-03-17T18:47:43.634351-06:00'
model: gpt-4-0125-preview
summary: "\u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\u09B0 \u09A4\
  \u09C1\u09B2\u09A8\u09BE \u09AE\u09BE\u09A8\u09C7 \u09A4\u09BE\u09A6\u09C7\u09B0\
  \ \u09B8\u09AE\u09CD\u09AA\u09B0\u09CD\u0995 \u0995\u09C0 \u09A4\u09BE \u09AA\u09B0\
  \u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\u09BE\u2014\u0995\u09CB\u09A8\u099F\u09BF\
  \ \u0986\u0997\u09C7\u09B0, \u09AA\u09B0\u09C7\u09B0 \u09A8\u09BE\u0995\u09BF \u09A0\
  \u09BF\u0995 \u0985\u09AA\u09B0\u099F\u09BF\u09B0 \u09AE\u09A4\u09CB\u0987."
title: "\u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u09A4\u09C1\u09B2\
  \u09A8\u09BE \u0995\u09B0\u09BE"
weight: 27
---

## কী এবং কেন?
দুটি তারিখের তুলনা মানে তাদের সম্পর্ক কী তা পরীক্ষা করা—কোনটি আগের, পরের নাকি ঠিক অপরটির মতোই? প্রোগ্রামাররা ডেডলাইন নির্বাহ, ইভেন্ট সূচিবদ্ধ করা এবং সময়-সংক্রান্ত ডেটা ট্র্যাক করার জন্য এটা করে থাকেন।

## কীভাবে:
Clojure তারিখ নির্বাহের জন্য Java interop সুবিধাগুলি ব্যবহার করে। চলুন হাত গুটিয়ে ডুব দেই:

```clojure
;; Java Date class ইম্পোর্ট করুন
(import java.util.Date)

;; দুটি তারিখের উদাহরণ তৈরি করুন
(def date1 (java.util.Date.))
(Thread/sleep 1000) ;; একটু অপেক্ষা করুন
(def date2 (java.util.Date.))

;; তারিখগুলি তুলনা করুন
(println (.before date1 date2)) ; সত্যি, date1 টি date2 এর আগে
(println (.after date1 date2))  ; মিথ্যা, date1 টি date2 এর পরে নয়
(println (.equals date1 date2)) ; মিথ্যা, date1 টি এবং date2 সমান নয়
```

নমুনা আউটপুট এইরকম দেখাবে, তবে সময়ের স্তম্ভগুলি ভিন্ন হবে:

```
সত্যি
মিথ্যা
মিথ্যা
```

## গভীর ডাইভ
অতীতে, Clojure ডেভেলপাররা প্রায়ই Java-র `Date` ব্যবহার করতেন তারিখের অপারেশনের জন্য, আগে দেখানো dot অপারেটর ব্যবহার করে। বিকল্পগুলোর মধ্যে `clj-time`, একটি Clojure লাইব্রেরি যা Joda-Time উপর নির্ভর করে।

`clj-time` ব্যবহার করার একটি উদাহরণ এরকম হবে:

```clojure
;; আপনার প্রজেক্ট ডিপেনডেন্সিতে clj-time যোগ করুন
(require '[clj-time.core :as time])
(require '[clj-time.coerce :as coerce])

;; দুটি তারিখ-সময়ের উদাহরণ তৈরি করুন
(def date-time1 (time/now))
(Thread/sleep 1000) ;; এক সেকেন্ড অপেক্ষা করুন
(def date-time2 (time/now))

;; clj-time ফাংশন ব্যবহার করে তুলনা করুন
(println (time/before? date-time1 date-time2)) ; সত্যি
(println (time/after? date-time1 date-time2))  ; মিথ্যা
(println (time/equal? date-time1 date-time2))  ; মিথ্যা
```

Clojure-র সময়ের উপর অবস্থান Java-র লাইব্রেরিগুলি ব্যবহার করা এবং clj-time জাভা-টাইমের সাথে ইন্টাগ্রেট করে একটি বেশি Clojure-ভাষ্যমূলক অভিজ্ঞতা প্রদান করে।

Java 8 থেকে, `java.time` প্যাকেজ—Joda-Time অনুপ্রাণিত—Java এবং অন্যান্য interop-এর মাধ্যমে Clojure-এ তারিখ এবং সময় নিয়ে কাজ করার জন্য পছন্দসই উপায় হয়ে উঠেছে। সময় অঞ্চলের মতো উন্নত ডিজাইন এবং অতিরিক্ত কার্যকারীতা `java.time` কে একটি শক্তিশালী পছন্দ করে তোলে।

## আরো দেখুন
- [Clojure-র Java Interop](https://clojure.org/reference/java_interop)
- [clj-time GitHub রিপোজিটরি](https://github.com/clj-time/clj-time)
- [Java তারিখ এবং সময় API গাইড](https://docs.oracle.com/javase/tutorial/datetime/)

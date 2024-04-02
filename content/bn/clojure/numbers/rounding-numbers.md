---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:14:08.120394-06:00
description: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE \u0997\u09CB\u09B2 \u0995\u09B0\
  \u09BE \u098F\u09B0 \u09AE\u09BE\u09A8\u09C7 \u09B9\u099A\u09CD\u099B\u09C7 \u098F\
  \u0995\u099F\u09BF \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u0995\u09C7 \u09A4\u09BE\
  \u09B0 \u09A8\u09BF\u0995\u099F\u09A4\u09AE \u09AA\u09C2\u09B0\u09CD\u09A3 \u09B8\
  \u0982\u0996\u09CD\u09AF\u09BE \u09AC\u09BE \u098F\u0995\u099F\u09BF \u09A8\u09BF\
  \u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09A6\u09B6\u09AE\u09BF\u0995 \u09A8\
  \u09BF\u09B0\u09CD\u09A3\u09BE\u09AF\u09BC\u0995\u09C7 \u09A0\u09BF\u0995 \u0995\
  \u09B0\u09BE\u0964 \u0986\u09AE\u09B0\u09BE \u09B8\u0982\u0996\u09CD\u09AF\u09BE\
  \ \u0997\u09CB\u09B2 \u0995\u09B0\u09BF \u098F\u099F\u09BF\u0995\u09C7 \u09AE\u09BE\
  \u09A8\u09AC \u09AA\u09BE\u09A0\u09AF\u09CB\u0997\u09CD\u09AF\u09A4\u09BE\u09B0\u2026"
lastmod: '2024-03-17T18:47:43.615789-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE \u0997\u09CB\u09B2 \u0995\u09B0\u09BE\
  \ \u098F\u09B0 \u09AE\u09BE\u09A8\u09C7 \u09B9\u099A\u09CD\u099B\u09C7 \u098F\u0995\
  \u099F\u09BF \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u0995\u09C7 \u09A4\u09BE\u09B0\
  \ \u09A8\u09BF\u0995\u099F\u09A4\u09AE \u09AA\u09C2\u09B0\u09CD\u09A3 \u09B8\u0982\
  \u0996\u09CD\u09AF\u09BE \u09AC\u09BE \u098F\u0995\u099F\u09BF \u09A8\u09BF\u09B0\
  \u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09A6\u09B6\u09AE\u09BF\u0995 \u09A8\u09BF\
  \u09B0\u09CD\u09A3\u09BE\u09AF\u09BC\u0995\u09C7 \u09A0\u09BF\u0995 \u0995\u09B0\
  \u09BE\u0964 \u0986\u09AE\u09B0\u09BE \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u0997\
  \u09CB\u09B2 \u0995\u09B0\u09BF \u098F\u099F\u09BF\u0995\u09C7 \u09AE\u09BE\u09A8\
  \u09AC \u09AA\u09BE\u09A0\u09AF\u09CB\u0997\u09CD\u09AF\u09A4\u09BE\u09B0\u2026"
title: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09A8\u09BF\u09B0\u09CD\u09A3\u09DF"
weight: 13
---

## কি এবং কেন?
সংখ্যা গোল করা এর মানে হচ্ছে একটি সংখ্যাকে তার নিকটতম পূর্ণ সংখ্যা বা একটি নির্দিষ্ট দশমিক নির্ণায়কে ঠিক করা। আমরা সংখ্যা গোল করি এটিকে মানব পাঠযোগ্যতার জন্য সহজ করতে, গণনার ভার হ্রাস করতে, অথবা নির্দিষ্ট সংখ্যাসূচক প্রয়োজনীয়তা পূরণ করতে।

## কিভাবে:
Clojure-এ, আমরা মূলত `Math/round`, `Math/floor`, এবং `Math/ceil` ব্যবহার করি:

```clojure
(Math/round 3.5) ; => 4
(Math/round 3.4) ; => 3

(Math/floor 3.7) ; => 3.0
(Math/ceil 3.2)  ; => 4.0
```

নির্দিষ্ট দশমিক স্থানের জন্য, আমরা গুণ, গোল করি, এবং ভাগ করি:

```clojure
(let [num 3.14159
      scale 1000]
  (/ (Math/round (* num scale)) scale)) ; => 3.142
```

## প্রগাঢ় অনুসন্ধান
জাদুকরী প্রোগ্রামিং ভাষার আগে, গোল করা ছিল একটি ম্যানুয়াল প্রক্রিয়া, চিন্তা করুন একটি সোরোবান বা কাগজের কথা। প্রোগ্রামিংয়ে, এটি ভাসমান-বিন্দু নির্ভুলতার সীমাবদ্ধতার জন্য সংখ্যা উপস্থাপনের জন্য অপরিহার্য।

গোল করার বিকল্পগুলি অন্তর্ভুক্ত আছে `BigDecimal` শ্রেণিটি নির্ভুলতা নিয়ন্ত্রণের জন্য বা `clojure.math.numeric-tower` মত লাইব্রেরি উন্নত গাণিতিক ফাংশনের জন্য। Clojure-এর `Math/round` জাভার `Math.round`, `Math/floor`, এবং `Math/ceil` ফাংশনের উপর নির্ভর করে, যার মানে এটি একই float এবং double নিয়মিত বৈশিষ্ট্যগুলি পায়।

বাস্তবায়নে, Clojure-এ যখন গোল করা হয়, মনে রাখবেন এটি স্বয়ংক্রিয়ভাবে ডাবল নির্ভুলতা ব্যবহার করে দশমিক নিয়ে কাজ করার সময়। গোল করার ত্রুটিগুলিতে সাবধান!

## আরো দেখুন
- Clojure গণিত API: [https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*)
- জাভা গণিত API: [https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html)
- ভাসমান-বিন্দু নির্ভুলতা বুঝতে: [https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)

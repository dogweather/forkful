---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:14:08.120394-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Clojure-\u098F, \u0986\u09AE\u09B0\
  \u09BE \u09AE\u09C2\u09B2\u09A4 `Math/round`, `Math/floor`, \u098F\u09AC\u0982 `Math/ceil`\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BF."
lastmod: '2024-03-17T18:47:43.615789-06:00'
model: gpt-4-0125-preview
summary: "Clojure-\u098F, \u0986\u09AE\u09B0\u09BE \u09AE\u09C2\u09B2\u09A4 `Math/round`,\
  \ `Math/floor`, \u098F\u09AC\u0982 `Math/ceil` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09BF."
title: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09A8\u09BF\u09B0\u09CD\u09A3\u09DF"
weight: 13
---

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

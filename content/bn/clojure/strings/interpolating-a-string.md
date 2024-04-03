---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:50:41.727445-06:00
description: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\
  \u09B0\u09AA\u09CB\u09B2\u09C7\u09B6\u09A8 \u0986\u09AE\u09BE\u09A6\u09C7\u09B0\u0995\
  \u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09AE\u09A7\u09CD\
  \u09AF\u09C7 \u09AD\u09C7\u09B0\u09BF\u09AF\u09BC\u09C7\u09AC\u09B2 \u09A2\u09C1\
  \u0995\u09BE\u09A4\u09C7 \u09A6\u09C7\u09AF\u09BC \u09AC\u09BF\u09B0\u0995\u09CD\
  \u09A4\u09BF \u099B\u09BE\u09A1\u09BC\u09BE\u0987\u0964 \u0995\u09C7\u09A8? \u09A1\
  \u09BE\u09AF\u09BC\u09A8\u09BE\u09AE\u09BF\u0995\u09BE\u09B2\u09BF \u099F\u09C7\u0995\
  \u09CD\u09B8\u099F \u09A8\u09BF\u09B0\u09CD\u09AE\u09BE\u09A3 \u0995\u09B0\u09BE\
  \u09B0 \u099C\u09A8\u09CD\u09AF\u2014\u09AF\u09BE \u09AA\u09C1\u09B0\u09CB\u09A8\
  \u09CB \u09B8\u09CD\u0995\u09C1\u09B2\u09C7\u09B0 \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982\u2026"
lastmod: '2024-03-17T18:47:43.605413-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\
  \u09B0\u09AA\u09CB\u09B2\u09C7\u09B6\u09A8 \u0986\u09AE\u09BE\u09A6\u09C7\u09B0\u0995\
  \u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09AE\u09A7\u09CD\
  \u09AF\u09C7 \u09AD\u09C7\u09B0\u09BF\u09AF\u09BC\u09C7\u09AC\u09B2 \u09A2\u09C1\
  \u0995\u09BE\u09A4\u09C7 \u09A6\u09C7\u09AF\u09BC \u09AC\u09BF\u09B0\u0995\u09CD\
  \u09A4\u09BF \u099B\u09BE\u09A1\u09BC\u09BE\u0987\u0964 \u0995\u09C7\u09A8."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\
  \u09AA\u09CB\u09B2\u09C7\u099F \u0995\u09B0\u09BE"
weight: 8
---

## কিভাবে:
```Clojure
;; `str` এবং `format` ব্যবহার করে মৌলিক
(def name "World")
(str "Hello, " name "!")  ; => "Hello, World!"

;; `format` ব্যবহার করে, printf-শৈলী ফরম্যাটিং এর অনুরূপ
(format "Goodbye, %s!" name)  ; => "Goodbye, World!"

;; Clojure এ অন্যান্য ভাষার মতো বিল্ট-ইন স্ট্রিং ইন্টারপোলেশন নেই,
;; কিন্তু আমরা `str` এবং `format` এর সাথে সৃজনশীল হতে পারি।
```

## গভীরে ডাইভ:
Clojure একটু ত্যাগী: কোনো বিল্ট-ইন স্ট্রিং ইন্টারপোলেশন নেই। তবে, ডায়নামিক স্ট্রিং-এর জন্য `str` এবং `format` হল প্রথম পছন্দ। মূল কাহিনী? Clojure-এর সহজাত দর্শন। এটি বিশ্বাস করে আমরা নিজেরা স্ট্রিং নির্মাণ করতে পারি।

বিকল্পের জন্যে, টেমপ্লেটিং বিশ্বে প্রবেশ করুন: `clostache` (Mustache-এর একটি Clojure বাস্তবায়ন) অথবা HTML প্রেক্ষাপটে `hiccup`। যখন `str` এবং `format` খুব প্রাথমিক মনে হয়, তখন এগুলো কাজে আসে।

অন্তরালে, `format` Java-এর `String.format`-এ নির্ভর করে, একটি তথ্য যা Clojure-এর জাভা ইন্টারপারাবিলিটি সুপারপাওয়ার প্রদর্শন করে। তাই, আপনি মিষ্টি পেলেন না হলেও, যখন প্রয়োজন হয় তখন আপনার কাছে জাভা-এর শক্তি আছে।

## দেখুন ও:
- `str` সম্পর্কে Clojure ডকস: https://clojuredocs.org/clojure.core/str
- `format` সম্পর্কে Clojure ডকস: https://clojuredocs.org/clojure.core/format
- clostache GitHub repo: https://github.com/fhd/clostache
- hiccup GitHub repo: https://github.com/weavejester/hiccup

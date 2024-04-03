---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:08:40.302708-06:00
description: "\u098F\u0995\u099F\u09BF \u09AA\u09BE\u09A0\u09CD\u09AF \u09AB\u09BE\
  \u0987\u09B2 \u09AA\u09A1\u09BC\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u0986\
  \u09AA\u09A8\u09BE\u09B0 \u09A1\u09BF\u09B8\u09CD\u0995\u09C7 \u09B8\u0982\u09B0\
  \u0995\u09CD\u09B7\u09BF\u09A4 \u09AB\u09BE\u0987\u09B2 \u09A5\u09C7\u0995\u09C7\
  \ \u09A1\u09C7\u099F\u09BE \u0986\u09AA\u09A8\u09BE\u09B0 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09C7 \u0986\u09A8\u09BE\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u0997\u09A3 \u09AE\u09CD\u09AF\u09BE\
  \u09A8\u09C1\u09AF\u09BC\u09BE\u09B2 \u0987\u09A8\u09AA\u09C1\u099F \u099B\u09BE\
  \u09A1\u09BC\u09BE\u0987 \u0995\u09A8\u09CD\u099F\u09C7\u09A8\u09CD\u099F \u09AA\
  \u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE \u09AC\u09BE\u2026"
lastmod: '2024-03-17T18:47:43.640345-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u09AA\u09BE\u09A0\u09CD\u09AF \u09AB\u09BE\u0987\
  \u09B2 \u09AA\u09A1\u09BC\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u0986\u09AA\
  \u09A8\u09BE\u09B0 \u09A1\u09BF\u09B8\u09CD\u0995\u09C7 \u09B8\u0982\u09B0\u0995\
  \u09CD\u09B7\u09BF\u09A4 \u09AB\u09BE\u0987\u09B2 \u09A5\u09C7\u0995\u09C7 \u09A1\
  \u09C7\u099F\u09BE \u0986\u09AA\u09A8\u09BE\u09B0 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09C7 \u0986\u09A8\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u0997\u09A3 \u09AE\u09CD\u09AF\u09BE\u09A8\
  \u09C1\u09AF\u09BC\u09BE\u09B2 \u0987\u09A8\u09AA\u09C1\u099F \u099B\u09BE\u09A1\
  \u09BC\u09BE\u0987 \u0995\u09A8\u09CD\u099F\u09C7\u09A8\u09CD\u099F \u09AA\u09CD\
  \u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE \u09AC\u09BE \u09AC\u09BF\u09B6\
  \u09CD\u09B2\u09C7\u09B7\u09A3, \u099F\u09BE\u09B8\u09CD\u0995 \u0985\u099F\u09CB\
  \u09AE\u09C7\u099F \u0995\u09B0\u09BE, \u09AC\u09BE \u0995\u09A8\u09AB\u09BF\u0997\
  \u09BE\u09B0\u09C7\u09B6\u09A8 \u09A1\u09C7\u099F\u09BE \u09AA\u09BE\u09B0\u09CD\
  \u09B8 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u099F\u09BE \u0995\
  \u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u09A8\u0964."
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\
  \u09BC\u09BE"
weight: 22
---

## কি এবং কেন?

একটি পাঠ্য ফাইল পড়া মানে হল আপনার ডিস্কে সংরক্ষিত ফাইল থেকে ডেটা আপনার প্রোগ্রামে আনা। প্রোগ্রামারগণ ম্যানুয়াল ইনপুট ছাড়াই কন্টেন্ট প্রক্রিয়া বা বিশ্লেষণ, টাস্ক অটোমেট করা, বা কনফিগারেশন ডেটা পার্স করার জন্য এটা করে থাকেন।

## কিভাবে:

```Clojure
;; পুরো ফাইলটি স্ট্রিং আকারে পড়ুন
(slurp "example.txt")

;; আউটপুট: "Hello, this is your file content!"

;; লাইন অনুযায়ী ফাইল পড়ুন
(with-open [reader (clojure.java.io/reader "example.txt")]
  (doseq [line (line-seq reader)]
    (println line)))

;; আউটপুট:
;; Hello,
;; this is your
;; file content!
```

## গভীর ডুব

ঐতিহ্যগতভাবে, প্রোগ্রামিং ভাষায় ফাইল পড়া ছিল বিস্তৃত কার্য যা ত্রুটি এবং সম্পদ পরিচালনার অনেক ধাপ নিয়ে। Clojure এ, আপনি `slurp` ফাংশনের সুবিধা পান, যা পুরো ফাইলের কন্টেন্ট এক লাইনে টেনে আনার এক উত্তম উপায়। লাইন দ্বারা লাইন পড়ার জন্য, `line-seq` এবং `with-open` এর সংমিশ্রণ ফাইল পরিচালনার জন্য কার্যকর এবং নিরাপদ নিশ্চিত করে। এটাও উল্লেখ করা জরুরি যে, `slurp` বড় ফাইলের জন্য আদর্শ নয় মেমোরি বৃত্তির কারণে। তখন `line-seq` দুর্দান্ত কাজ করে, কারণ এটি ফাইলটি অলসভাবে পড়ে, এক লাইন করে।

Clojure এ ফাইল পড়ার অন্যান্য উপায়ের মধ্যে রয়েছে `clojure.java.io/file` এর সাথে যেমন `reader` ফাংশন এবং `with-open` এর মত কাঠামোর সাথে সম্পদ ম্যানুয়ালি পরিচালনা। এখানে ট্রেড-অফ হল সহজ ব্যবহারের মাঝে (`slurp`) এবং সূক্ষ্ম নিয়ন্ত্রণ সহ সম্পদ নিরাপত্তার মাঝে (`with-open` এবং `reader`)।

বাস্তবায়নের দিক থেকে, Clojure এর পদ্ধতি Java এর IO শ্রেণীগুলির মধ্যে ভিত্তি করে তৈরি, সুতরাং যখন আপনি Clojure এ ফাইলের সঙ্গে কাজ করছেন, আপনি Java এর পরীক্ষিত, ভাল পরীক্ষিত লাইব্রেরির সাথে কাজ করছেন, যা একটি কার্যকরী উপলক্ষে মোড়ানো। সম্পদের উপর সবসময় নজর রাখুন: খোলা ফাইলগুলি হ্যান্ডেল এবং মেমোরি খরচ করে, তাই পরিষ্কার ফাইল পরিচালনা একটি পরিচ্ছন্ন অভ্যাস।

## দেখুন এছাড়াও

- `slurp` জন্য ClojureDocs: https://clojuredocs.org/clojure.core/slurp
- `line-seq` জন্য ClojureDocs: https://clojuredocs.org/clojure.core/line-seq
- Clojure এ Java interop: https://clojure.org/reference/java_interop
- Clojure এ ফাইলের সাথে কাজ (Practical.li): https://practical.li/clojure/working-with-files.html

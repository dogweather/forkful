---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:38:20.712715-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Clojure 'clojure.lang.Numbers'\
  \ \u0987\u0989\u099F\u09BF\u09B2\u09BF\u099F\u09BF \u0995\u09CD\u09B2\u09BE\u09B8\
  \u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u099C\u099F\u09BF\u09B2\
  \ \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u0985\u09A8\
  \u09CD\u09A4\u09B0\u09CD\u09AD\u09C1\u0995\u09CD\u09A4 \u09B8\u09AE\u09B0\u09CD\u09A5\
  \u09A8 \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964 `complex` \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u099C\u099F\u09BF\u09B2\
  \ \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09A4\u09C8\u09B0\u09BF\u2026"
lastmod: '2024-03-17T18:47:43.614736-06:00'
model: gpt-4-0125-preview
summary: "Clojure 'clojure.lang.Numbers' \u0987\u0989\u099F\u09BF\u09B2\u09BF\u099F\
  \u09BF \u0995\u09CD\u09B2\u09BE\u09B8\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\
  \u09AE\u09C7 \u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09AD\u09C1\u0995\
  \u09CD\u09A4 \u09B8\u09AE\u09B0\u09CD\u09A5\u09A8 \u09AA\u09CD\u09B0\u09A6\u09BE\
  \u09A8 \u0995\u09B0\u09C7\u0964 `complex` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09C7 \u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\
  \ \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09C1\u09A8 \u098F\u09AC\u0982 \u0985\u0999\
  \u09CD\u0995\u0997\u09A3\u09A8\u09BE \u0995\u09B0\u09C1\u09A8\u0964."
title: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 14
---

## কিভাবে:
Clojure 'clojure.lang.Numbers' ইউটিলিটি ক্লাসের মাধ্যমে জটিল সংখ্যার জন্য অন্তর্ভুক্ত সমর্থন প্রদান করে। `complex` ব্যবহার করে জটিল সংখ্যা তৈরি করুন এবং অঙ্কগণনা করুন।

```clojure
;; জটিল সংখ্যা তৈরি করা
(def a (clojure.lang.Numbers/complex 3 4))  ; 3 + 4i
(def b (clojure.lang.Numbers/complex 1 -1)) ; 1 - i

;; যোগ
(+ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5c6cfe9 "4 + 3i"]

;; বিয়োগ
(- a b) ;=> #object[clojure.lang.Numbers.Complex 0x5e51118 "2 + 5i"]

;; গুণ
(* a b) ;=> #object[clojure.lang.Numbers.Complex 0x6ec3f0df "7 + i"]

;; ভাগ
(/ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5db0cd10 "3.5 + 3.5i"]

;; সহগ (Conjugate)
(.conjugate a) ;=> #object[clojure.lang.Numbers.Complex 0x47c6e076 "3 - 4i"]
```

## গভীর ডুব
১৮শ শতাব্দীতে গাউস এবং ইউলারের মতো গণিতবিদরা জটিল সংখ্যার আনুষ্ঠানিকীকরণ করেন। প্রাথমিকভাবে এটি সন্দেহের মুখোমুখি হলেও, এখন এটি আধুনিক বিজ্ঞান এবং প্রকৌশলে অপরিহার্য হয়ে উঠেছে। Clojure কিছু ভাষা (যেমন, পাইথন) এর মতো জটিল সংখ্যার নিজস্ব ধরণ নেই, তবে অন্তর্ভুক্ত Java ইন্টারঅপ মাধ্যমে দরকারী অপারেশন সামলানো যেতে পারে `clojure.lang.Numbers` ক্লাসের মাধ্যমে।

Java-র `java.lang.Complex` একটি শক্তিশালী বিকল্প, যা আরো বৈশিষ্ট্য এবং সম্ভাব্য অপ্টিমাইজেশন প্রদান করে। Clojure-র হোস্ট ইন্টারঅপারেবিলিটির মাধ্যমে Java লাইব্রেরিগুলির সাথে কাজ করা সহজ হয়ে উঠেছে।

প্রচ্ছন্নভাবে, জটিল সংখ্যার অঙ্কগণনা বাস্তব এবং কল্পিত অংশগুলি যোগ এবং গুণ করা এবং মূল নিয়ম যে `i^2 = -1` এর সাথে জড়িত। জটিল সংখ্যার ভাগকরণ আরও জটিল হতে পারে, সাধারণত সহগ ব্যবহার করে জটিল সংখ্যার দ্বারা ভাগ এড়াতে প্রয়োজন হয়।

## আরও দেখুন
- ClojureDocs, দ্রুত রেফারেন্সের জন্য: https://clojuredocs.org/
- `java.lang.Complex`-এর জন্য Java API: https://docs.oracle.com/javase/8/docs/api/java/lang/Complex.html
- গাণিতিক কৌতুহলীদের জন্য জটিল সংখ্যার উপর Wikipedia পৃষ্ঠা: https://en.wikipedia.org/wiki/Complex_number

---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:39:54.241966-06:00
description: "Clojure \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \ \u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\
  \u09B2 \u09B2\u09BF\u0996\u09BE \u09AE\u09BE\u09A8\u09C7 \u09A4\u09A5\u09CD\u09AF\
  \ \u0986\u09AA\u09A8\u09BE\u09B0 \u0985\u09CD\u09AF\u09BE\u09AA\u09CD\u09B2\u09BF\
  \u0995\u09C7\u09B6\u09A8\u09C7\u09B0 \u09AC\u09BE\u0987\u09B0\u09C7 \u09B8\u0982\
  \u09B0\u0995\u09CD\u09B7\u09A3, \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\
  \u09B6\u09A8, \u09B2\u0997\u09BF\u0982 \u0985\u09A5\u09AC\u09BE \u0987\u09A8\u09CD\
  \u099F\u09BE\u09B0-\u09AA\u09CD\u09B0\u09B8\u09C7\u09B8 \u0995\u09AE\u09BF\u0989\
  \u09A8\u09BF\u0995\u09C7\u09B6\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AB\
  \u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF\u2026"
lastmod: '2024-03-17T18:47:43.641570-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u098F\
  \u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2\
  \ \u09B2\u09BF\u0996\u09BE \u09AE\u09BE\u09A8\u09C7 \u09A4\u09A5\u09CD\u09AF \u0986\
  \u09AA\u09A8\u09BE\u09B0 \u0985\u09CD\u09AF\u09BE\u09AA\u09CD\u09B2\u09BF\u0995\u09C7\
  \u09B6\u09A8\u09C7\u09B0 \u09AC\u09BE\u0987\u09B0\u09C7 \u09B8\u0982\u09B0\u0995\
  \u09CD\u09B7\u09A3, \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8\
  , \u09B2\u0997\u09BF\u0982 \u0985\u09A5\u09AC\u09BE \u0987\u09A8\u09CD\u099F\u09BE\
  \u09B0-\u09AA\u09CD\u09B0\u09B8\u09C7\u09B8 \u0995\u09AE\u09BF\u0989\u09A8\u09BF\
  \u0995\u09C7\u09B6\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AB\u09BE\u0987\
  \u09B2 \u09A4\u09C8\u09B0\u09BF\u2026"
title: "\u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\
  \u0987\u09B2 \u09B2\u09BF\u0996\u09BE"
weight: 24
---

## কি এবং কেন?

Clojure ব্যবহার করে একটি টেক্সট ফাইল লিখা মানে তথ্য আপনার অ্যাপ্লিকেশনের বাইরে সংরক্ষণ, কনফিগারেশন, লগিং অথবা ইন্টার-প্রসেস কমিউনিকেশনের জন্য ফাইল তৈরি করা বা সংশোধন করা। প্রোগ্রামাররা অ্যাপ্লিকেশনের স্টেট, কনফিগারেশনস বা বিভিন্ন প্রোগ্রাম অংশে অথবা আলাদা প্রোগ্রামে তথ্য ভাগাভাগি করার জন্য এই কাজ করে থাকেন।

## কিভাবে:

### Clojure-এর বিল্ট-ইন ফাংশন ব্যবহার করে ফাইলে টেক্সট লেখা

`spit` ফাংশনটি Clojure ব্যবহার করে একটি ফাইলে টেক্সট লেখার সবচেয়ে সহজ উপায়। এটি দুটি আর্গুমেন্ট নেয়: ফাইলের পাথ এবং লেখার জন্য স্ট্রিং। যদি ফাইলটি না থাকে, `spit` এটি তৈরি করবে। যদি থাকে, `spit` এটিকে ওভাররাইট করবে।

```clojure
(spit "example.txt" "Hello, world!")
```

একটি বিদ্যমান ফাইলে টেক্সট যোগ করতে আপনি `:append` অপশন সহ `spit` ফাংশনটি ব্যবহার করতে পারেন।

```clojure
(spit "example.txt" "\nLet's add this new line." :append true)
```

এই স্নিপেটগুলি চালানোর পর, "example.txt" ফাইলে থাকবে:

```
Hello, world!
Let's add this new line.
```

### থার্ড-পার্টি লাইব্রেরিগুলি ব্যবহার করা

যদিও Clojure-এর বিল্ট-ইন ক্ষমতাসমূহ প্রায়ই যথেষ্ট, কমিউনিটি আরও জটিল অথবা নির্দিষ্ট কাজের জন্য শক্তিশালী লাইব্রেরিগুলি বিকাশ করেছে। ফাইল I/O-এর জন্য একটি জনপ্রিয় লাইব্রেরি হল `clojure.java.io`, যা ফাইল হ্যান্ডলিং এর জন্য আরও Java-লাইক প্রোচেস প্রদান করে।

`clojure.java.io` ব্যবহার করে একটি ফাইলে লেখার জন্য, প্রথমে আপনাকে এটি আমদানি করতে হবে:

```clojure
(require '[clojure.java.io :as io])
```

এরপর, আপনি `writer` ফাংশন ব্যবহার করে একটি রাইটার অবজেক্ট পেতে পারেন, এবং `spit` ফাংশন (অথবা অন্যান্য যেমন `print`, `println`) ব্যবহার করে ফাইলে লিখতে পারেন:

```clojure
(with-open [w (io/writer "example_with_io.txt")]
  (.write w "This is written using clojure.java.io"))
```

এটি "example_with_io.txt" কে (যদি আগে থেকেই থাকে তাহলে ওভাররাইট করে) নিম্নোক্ত টেক্সট সহ তৈরি করবে:

```
This is written using clojure.java.io
```

মনে রাখবেন: `with-open` লেখার পর ফাইলটি যাতে যথাযথভাবে বন্ধ হয় তা নিশ্চিত করে, যাতে সম্ভাব্য রিসোর্স লিক এড়ানো যায়।

---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:16:03.538602-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Clojure-\u098F, \u0986\u09AE\u09B0\
  \u09BE \u099F\u09C7\u0995\u09CD\u09B8\u099F \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\
  \u09A7\u09BE\u09A8 \u098F\u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\
  \u09A5\u09BE\u09AA\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF `clojure.string/replace`\
  \ \u09AB\u09BE\u0982\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09BF\u0964 \u0995\u09BF\u099B\u09C1 \u0995\u09CB\u09A1 \u09A6\u09BF\u09AF\
  \u09BC\u09C7 \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF \u09AC\u09BF\u09B7\u09AF\u09BC\
  \u09C7 \u09AF\u09BE\u0987."
lastmod: '2024-03-17T18:47:43.604303-06:00'
model: gpt-4-0125-preview
summary: "Clojure-\u098F, \u0986\u09AE\u09B0\u09BE \u099F\u09C7\u0995\u09CD\u09B8\u099F\
  \ \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\u09A7\u09BE\u09A8 \u098F\u09AC\u0982 \u09AA\
  \u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\u09A5\u09BE\u09AA\u09A8\u09C7\u09B0 \u099C\u09A8\
  \u09CD\u09AF `clojure.string/replace` \u09AB\u09BE\u0982\u09B6\u09A8 \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BF\u0964 \u0995\u09BF\u099B\u09C1\
  \ \u0995\u09CB\u09A1 \u09A6\u09BF\u09AF\u09BC\u09C7 \u09B8\u09B0\u09BE\u09B8\u09B0\
  \u09BF \u09AC\u09BF\u09B7\u09AF\u09BC\u09C7 \u09AF\u09BE\u0987."
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\u09A7\
  \u09BE\u09A8 \u098F\u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\u09A5\
  \u09BE\u09AA\u09A8"
weight: 10
---

## কিভাবে:
Clojure-এ, আমরা টেক্সট অনুসন্ধান এবং প্রতিস্থাপনের জন্য `clojure.string/replace` ফাংশন ব্যবহার করি। কিছু কোড দিয়ে সরাসরি বিষয়ে যাই:

```clojure
(require '[clojure.string :as str])

;; মৌলিক প্রতিস্থাপন
(str/replace "I like apples" "apples" "oranges")
;; => "I like oranges"

;; সব স্বরবর্ণ প্রতিস্থাপনের জন্য একটি রেগুলার এক্সপ্রেশন ব্যবহার করা
(str/replace "Hello, World!" "[AEIOUaeiou]" "*")
;; => "H*ll*, W*rld!"

;; গতিশীল পরিবর্তনের জন্য কোন ফাংশনের সাথে প্রতিস্থাপন
(str/replace "I have 2 apples and 5 bananas"
             #"\d+"
             (fn [match] (str (inc (Integer/parseInt match)))))
;; => "I have 3 apples and 6 bananas"
```

এটা এতটাই সিম্পল। এটি রান করুন, এবং আপনি আপনার REPL-এ পরিবর্তনগুলি সরাসরি দেখতে পারবেন।

## গভীরে ডুব দেওয়া
টেক্সটে অনুসন্ধান এবং প্রতিস্থাপন নতুন নয়। এটি কম্পিউটিং-এ প্রাচীন। `sed` এর মতো প্রাথমিক এডিটরগুলো থেকে আমরা এটি পেয়েছি ইউনিক্সে। তারপর থেকে আমরা অনেক দূরে এসেছি।

Clojure, JVM-এ থাকার কারণে, মানে আপনার অধীনে জাভার রেগুলার এক্সপ্রেশনের শক্তি আছে। কর্মক্ষমতার দিক থেকে, এটি দ্রুত স্ক্রিপ্টের জন্য চমৎকার, তবে মনে রাখবেন, বড় পরিসরে টেক্সট প্রক্রিয়াকরণের অতিরিক্ত ব্যবহার কর্মক্ষমতাকে ক্ষতিগ্রস্থ করতে পারে।

বিকল্প হিসেবে, `clojure.string/replace` ছাড়াও, রেগুলার এক্সপ্রেশন-ভিত্তিক লাইব্রেরি বা আপনি যদি সাহসী বোধ করেন তবে আপনার কাস্টম ফাংশন লেখাও বিবেচনা করতে পারেন। যদি আপনি কেবল একবারের পরিবর্তনের দরকার হয়, তবে `replace-first` সম্পর্কে ভাবুন।

কার্যত, Clojure-এর অপরিবর্তনীয়তা প্রতি প্রতিস্থাপনের ফলে একটি নতুন স্ট্রিং তৈরি হয়। অপরিবর্তনীয় স্ট্রিংগুলি মানে কম বাগ এবং অপ্রত্যাশিত ঘটনা।

## আরও দেখুন
আরও গভীরে ডুব দিতে, এই সম্পদগুলি দেখুন:

- Clojure-এর `clojure.string` [API ডকুমেন্টেশন](https://clojuredocs.org/clojure.string/replace)
- রেগুলার এক্সপ্রেশনের উপর, জাভার [Pattern ক্লাস](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html)

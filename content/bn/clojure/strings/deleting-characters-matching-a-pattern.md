---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:14.336212-06:00
description: "\u0995\u09CB\u09A8\u09CB \u09A7\u09BE\u09B0\u09A3\u09BE\u09B0 \u09B8\
  \u0999\u09CD\u0997\u09C7 \u09AE\u09C7\u09B2\u09C7 \u098F\u09AE\u09A8 \u0985\u0995\
  \u09CD\u09B7\u09B0 \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\u09BE \u09AE\u09BE\
  \u09A8\u09C7 \u09B9\u099A\u09CD\u099B\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09CD\
  \u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A8\u09BF\u09B0\u09CD\
  \u09A6\u09BF\u09B7\u09CD\u099F \u0995\u09CD\u09B0\u09AE\u09C7\u09B0 \u0985\u0982\
  \u09B6 \u09AC\u09BE\u09A6 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE\u0964 \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A1\u09C7\u099F\
  \u09BE \u09AA\u09B0\u09BF\u09B7\u09CD\u0995\u09BE\u09B0 \u0995\u09B0\u09BE, \u09AB\
  \u09B0\u09AE\u09CD\u09AF\u09BE\u099F \u09AA\u09CD\u09B0\u09AF\u09CB\u099C\u09CD\u09AF\
  \u2026"
lastmod: '2024-03-17T18:47:43.603249-06:00'
model: gpt-4-0125-preview
summary: "\u0995\u09CB\u09A8\u09CB \u09A7\u09BE\u09B0\u09A3\u09BE\u09B0 \u09B8\u0999\
  \u09CD\u0997\u09C7 \u09AE\u09C7\u09B2\u09C7 \u098F\u09AE\u09A8 \u0985\u0995\u09CD\
  \u09B7\u09B0 \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\u09BE \u09AE\u09BE\u09A8\
  \u09C7 \u09B9\u099A\u09CD\u099B\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\
  \u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\
  \u09BF\u09B7\u09CD\u099F \u0995\u09CD\u09B0\u09AE\u09C7\u09B0 \u0985\u0982\u09B6\
  \ \u09AC\u09BE\u09A6 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A1\u09C7\u099F\u09BE\
  \ \u09AA\u09B0\u09BF\u09B7\u09CD\u0995\u09BE\u09B0 \u0995\u09B0\u09BE, \u09AB\u09B0\
  \u09AE\u09CD\u09AF\u09BE\u099F \u09AA\u09CD\u09B0\u09AF\u09CB\u099C\u09CD\u09AF\
  \ \u0995\u09B0\u09BE, \u0985\u09A5\u09AC\u09BE \u0985\u09A8\u09BE\u0995\u09BE\u0999\
  \u09CD\u0996\u09BF\u09A4 \u09A4\u09A5\u09CD\u09AF \u09AE\u09C1\u099B\u09C7 \u09AB\
  \u09C7\u09B2\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u099F\u09BF \u0995\u09B0\
  \u09C7 \u09A5\u09BE\u0995\u09C7\u09A8\u0964."
title: "\u098F\u0995\u099F\u09BF \u09A8\u09AE\u09C1\u09A8\u09BE \u09AE\u09C7\u09B2\
  \u09C7 \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09AE\u09C1\u099B\
  \u09C7 \u09AB\u09C7\u09B2\u09BE"
weight: 5
---

## কিভাবে:
Clojure-এ একটি প্যাটার্ন ব্যবহার করে অক্ষর মুছে ফেলার জন্য, আপনাকে নিয়মিত এক্সপ্রেশন `re-seq`, `re-find`, অথবা `re-matches` ফাংশনগুলির সাথে `clojure.string/replace` ব্যবহার করতে হবে।

```Clojure
(require '[clojure.string :as str])

;; একটি স্ট্রিং থেকে সমস্ত ডিজিট সরানো
(str/replace "He110 W0rld" #"\d+" "")
;; => "He Wrd"

;; নির্দিষ্ট বিশেষ অক্ষর সরানো
(str/replace "Hello, World! #Clojure" #"[,!#]" "")
;; => "Hello World Clojure"

;; শুধুমাত্র শব্দ অক্ষর এবং স্পেস রাখুন
(str/replace "Email@Example.com" #"[^\w\s]+" "")
;; => "EmailExamplecom"
```

## গভীরে ডুব:
Clojure, এর Lisp ধর্মানুষ্ঠানকে প্রতিফলিত করে, প্রতীকমূলক প্রক্রিয়াকরণে চমৎকার এবং এটি প্যাটার্ন-ম্যাচিংকে সহজ করে তোলে। ২০০৭ সালে প্রবর্তিত, এটি জাভা ভার্চুয়াল মেশিনের (JVM) ক্ষমতার উপর গড়ে উঠেছে, নিয়মিত এক্সপ্রেশনের জন্য জাভার শক্তিশালী `Pattern` ক্লাস ব্যবহার করে।

রেগেক্সের বিকল্প হিসেবে ম্যানুয়াল স্ট্রিং ইটারেশন এবং ম্যানিপুলেশন রয়েছে, কিন্তু এগুলি প্রায়ই বেশি বাচাল এবং ত্রুটিপূর্ণ হয়। `clojure.spec` মতো লাইব্রেরিগুলি সরাসরি মুছে ফেলা ছাড়াই প্যাটার্নের বিরুদ্ধে ডেটা যাচাই করতে এবং তার সাথে মিলিয়ে দিতে সহায়তা করতে পারে।

মুছে ফেলা অপারেশনগুলি সাধারণত খুবই দক্ষ, কিন্তু রেগেক্সের জটিলতার বিষয়ে সচেতন থাকুন, যা একটি O(n) টাস্ককে আরও খারাপ করে তুলতে পারে। Clojure-র অপরিবর্তনশীল স্ট্রিং মানে হচ্ছে প্রতিটি `replace` একটি নতুন স্ট্রিং তৈরি করে, যা মেমরি-সংবেদনশীল অ্যাপ্লিকেশনগুলির জন্য বিবেচনার যোগ্য।

## আরও দেখুন
- [Clojure-র স্ট্রিং API](https://clojure.github.io/clojure/clojure.string-api.html)
- [জাভা Pattern ক্লাস](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html)
- [Regular-Expressions.info](https://www.regular-expressions.info/)
- [clojure.spec](https://clojure.org/guides/spec)

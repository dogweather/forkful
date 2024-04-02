---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:09:49.556603-06:00
description: "\u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\
  \u09B0\u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F \u09AA\u09A1\u09BC\u09BE\u09B0\
  \ \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u098F\u0995\u099F\u09BF \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\u09C0\u09B0 \u099F\
  \u09BE\u09B0\u09CD\u09AE\u09BF\u09A8\u09BE\u09B2 \u0995\u09AE\u09BE\u09A8\u09CD\u09A1\
  \ \u09A5\u09C7\u0995\u09C7 \u09A4\u09A5\u09CD\u09AF \u09AA\u09C7\u09A4\u09C7 \u09AA\
  \u09BE\u09B0\u09C7\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\
  \u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09C7\u09A8 \u0995\u09CB\u09A1\
  \ \u09A8\u09BF\u099C\u09C7\u0987 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8\
  \ \u09A8\u09BE\u2026"
lastmod: '2024-03-17T18:47:43.638088-06:00'
model: gpt-4-0125-preview
summary: "\u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\u09B0\
  \u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F \u09AA\u09A1\u09BC\u09BE\u09B0\
  \ \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u098F\u0995\u099F\u09BF \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\u09C0\u09B0 \u099F\
  \u09BE\u09B0\u09CD\u09AE\u09BF\u09A8\u09BE\u09B2 \u0995\u09AE\u09BE\u09A8\u09CD\u09A1\
  \ \u09A5\u09C7\u0995\u09C7 \u09A4\u09A5\u09CD\u09AF \u09AA\u09C7\u09A4\u09C7 \u09AA\
  \u09BE\u09B0\u09C7\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\
  \u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09C7\u09A8 \u0995\u09CB\u09A1\
  \ \u09A8\u09BF\u099C\u09C7\u0987 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8\
  \ \u09A8\u09BE\u2026"
title: "\u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\u09B0\
  \u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F\u0997\u09C1\u09B2\u09BF \u09AA\u09A1\
  \u09BC\u09BE"
weight: 23
---

## কি ও কেন?

কমান্ড লাইন আর্গুমেন্ট পড়ার মাধ্যমে একটি প্রোগ্রাম সরাসরি ব্যবহারকারীর টার্মিনাল কমান্ড থেকে তথ্য পেতে পারে। প্রোগ্রামাররা এটি করেন কোড নিজেই পরিবর্তন না করে প্রোগ্রামের আচরণকে কাস্টমাইজ করার জন্য।

## কিভাবে:

ক্লোজারে, আপনি `*command-line-args*` দিয়ে কমান্ড লাইন আর্গুমেন্টগুলি ধরতে পারেন। এখানে একটি সহজ উদাহরণ দেয়া হল:

```clojure
;; ধরুন এই কোড `echo.clj` নামের একটি ফাইলে রাখা হয়েছে

(defn -main [& args]
  (println "আপনি প্রবেশ করেছেন:" args))

;; চালানোর জন্য: `clojure echo.clj arg1 arg2 arg3`
```

নমুনা আউটপুট:

```
আপনি প্রবেশ করেছেন: (arg1 arg2 arg3)
```

তাদের প্রক্রিয়া করার প্রয়োজন? ক্লোজারের সংগ্রহ ফাংশনগুলি ব্যবহার করুন।

```clojure
(defn -main [& args]
  (let [processed-args (mapv str/upper-case args)]
    (println "আপার কেসে:" processed-args)))

;; এখন, চালানো `clojure echo.clj hello world` আউটপুট দেবে:
```

নমুনা আউটপুট:

```
আপার কেসে: ["HELLO" "WORLD"]
```

## গভীর ডুব

`*command-line-args*` হল ক্লোজারে একটি ভেরিয়েবল, যা স্ক্রিপ্টে পাস করা আর্গুমেন্টগুলির একটি অনুক্রমে সেট করা হয়। এটি ক্লোজারের প্রারম্ভিক দিনগুলি থেকেই আছে, যা দেখায় ক্লোজার কমান্ড লাইন আর্গুমেন্টগুলিকে প্রথম শ্রেণীর নাগরিক হিসেবে বিবেচনা করে।

বিকল্পগুলি? জাভার কমান্ড লাইন আর্গুমেন্টগুলি ধরার যন্ত্রণাও ধন্যবাদ ক্লোজারে কাজ করে ইন্টারঅপারেবিলিটির কারণে। তবে এটি বেশি বাচাল।

বাস্তবায়নের বিস্তারিত বিবরণী অনুসারে, ক্লোজার শুরু হওয়ার সময়, এটি আর্গুমেন্টগুলি পার্স করে এবং তাদেরকে `*command-line-args*`-এ সংরক্ষণ করে। তারপর আপনার স্ক্রিপ্টটি তাদের সাথে যা খুশি করতে পারে—পার্স করা, উপেক্ষা করা, রূপান্তর করা, আপনি বলুন।

## আরও দেখুন

- অফিসিয়াল ক্লোজার CLI টুলস: https://clojure.org/guides/deps_and_cli
- ক্লোজার থেকে শুরু: কমান্ড-লাইন স্ক্রিপ্টিং: https://aphyr.com/posts/305-clojure-from-the-ground-up-command-line
- ক্লোজারডক্স অন *command-line-args*: https://clojuredocs.org/clojure.core/*command-line-args*

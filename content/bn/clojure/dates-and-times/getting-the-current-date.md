---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:33.427339-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: #."
lastmod: '2024-03-17T18:47:43.632308-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u09AA\u09C7\u09A4\u09C7"
weight: 29
---

## কিভাবে:


### জাভা ইন্টারঅপ ব্যবহার করে
Clojure এর জাভার সাথে নির্বিঘ্ন ইন্টারপেরিবিলিটি আপনাকে সরাসরি জাভা ডেট-টাইম API এ ঢুকতে দেয়। এখানে আপনি কিভাবে সাম্প্রতিক তারিখ পেতে পারেন:

```clojure
(import java.time.LocalDate)

(defn get-current-date []
  (str (LocalDate/now)))

;; নমুনা আউটপুট
(get-current-date) ; "2023-04-15"
```

### clj-time লাইব্রেরি ব্যবহার করে
একটি আরও আদি Clojure সমাধানের জন্য, আপনি `clj-time` লাইব্রেরি বেছে নিতে পারেন, যা হল Joda-Time এর একটি র‍্যাপার, তবে অধিকাংশ নতুন প্রকল্পের জন্য, বিল্ট-ইন জাভা 8 ডেট-টাইম API সুপারিশ করা হয়। তবুও, যদি আপনি `clj-time` প্রেফার করেন অথবা প্রয়োজনীয় মনে করেন:

প্রথমে, `clj-time` আপনার প্রজেক্টের ডিপেন্ডেন্সিজে যোগ করুন। আপনার `project.clj` তে অন্তর্ভুক্ত করুন:

```clojure
[clj-time "0.15.2"]
```

তারপর, এটি ব্যবহার করে সাম্প্রতিক তারিখ নিন:

```clojure
(require '[clj-time.core :as time])

(defn get-current-date-clj-time []
  (str (time/now)))

;; নমুনা আউটপুট
(get-current-date-clj-time) ; "2023-04-15T12:34:56.789Z"
```

উভয় পদ্ধতিই Clojure তে সাম্প্রতিক তারিখ পেতে দ্রুত, কার্যকরী উপায় প্রদান করে, অধস্তন জাভা প্ল্যাটফর্মের শক্তি বা একটি Clojure-বিশেষ লাইব্রেরির সুবিধা কাজে লাগানো হয়।

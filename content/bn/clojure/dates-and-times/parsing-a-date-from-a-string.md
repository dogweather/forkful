---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:05:44.247254-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u0995\u09CD\u09B2\u09CB\u099C\
  \u09BE\u09B0 \u098F\u0995\u099F\u09BF JVM \u09AD\u09BE\u09B7\u09BE \u09B9\u0993\u09AF\
  \u09BC\u09BE\u09AF\u09BC, \u0986\u09AA\u09A8\u09BF \u09B8\u09B0\u09BE\u09B8\u09B0\
  \u09BF \u099C\u09BE\u09AD\u09BE\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u098F\u09AC\
  \u0982 \u09B8\u09AE\u09AF\u09BC\u09C7\u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\
  \u09C7\u09B0\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\
  \u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u0986\u09B8\u09C1\u09A8 \u099C\u09BE\
  \u09AD\u09BE\u09B0 \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09A8\u09BF\u09B0\u09CD\u09AE\
  \u09BF\u09A4 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u0985\u09AA\u09BE\u09B0\u09C7\u09B6\
  \u09A8\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09B6\u09C1\u09B0\u09C1\u2026"
lastmod: '2024-03-17T18:47:43.631242-06:00'
model: gpt-4-0125-preview
summary: "\u0995\u09CD\u09B2\u09CB\u099C\u09BE\u09B0 \u098F\u0995\u099F\u09BF JVM\
  \ \u09AD\u09BE\u09B7\u09BE \u09B9\u0993\u09AF\u09BC\u09BE\u09AF\u09BC, \u0986\u09AA\
  \u09A8\u09BF \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF \u099C\u09BE\u09AD\u09BE\u09B0\
  \ \u09A4\u09BE\u09B0\u09BF\u0996 \u098F\u09AC\u0982 \u09B8\u09AE\u09AF\u09BC\u09C7\
  \u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\
  \u0964 \u0986\u09B8\u09C1\u09A8 \u099C\u09BE\u09AD\u09BE\u09B0 \u0985\u09A8\u09CD\
  \u09A4\u09B0\u09CD\u09A8\u09BF\u09B0\u09CD\u09AE\u09BF\u09A4 \u0987\u09A8\u09CD\u099F\
  \u09BE\u09B0\u0985\u09AA\u09BE\u09B0\u09C7\u09B6\u09A8\u09C7\u09B0 \u09B8\u09BE\u09A5\
  \u09C7 \u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09BF \u098F\u09AC\u0982 \u098F\u09B0\
  \u09AA\u09B0 \u0995\u09CD\u09B2\u09CB\u099C\u09BE\u09B0\u09C7 \u0986\u09B0\u0993\
  \ \u0985\u09AD\u09CD\u09AF\u09A8\u09CD\u09A4\u09B0\u09C0\u09A8 \u09B8\u09AE\u09BE\
  \u09A7\u09BE\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u099C\u09A8\u09AA\u09CD\
  \u09B0\u09BF\u09AF\u09BC \u09A4\u09C3\u09A4\u09C0\u09AF\u09BC-\u09AA\u0995\u09CD\
  \u09B7\u09C7\u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF, clj-time\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BE \u09AF\u09BE\u09AF\
  \u09BC \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u09A4\u09BE \u09A6\u09C7\u0996\u09AC\
  \u09CB\u0964\n\n#."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A4\
  \u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 30
---

## কিভাবে:
ক্লোজার একটি JVM ভাষা হওয়ায়, আপনি সরাসরি জাভার তারিখ এবং সময়ের লাইব্রেরি ব্যবহার করতে পারেন। আসুন জাভার অন্তর্নির্মিত ইন্টারঅপারেশনের সাথে শুরু করি এবং এরপর ক্লোজারে আরও অভ্যন্তরীন সমাধানের জন্য জনপ্রিয় তৃতীয়-পক্ষের লাইব্রেরি, clj-time ব্যবহার করা যায় কিভাবে তা দেখবো।

### জাভা ইন্টারঅপ ব্যবহার করে
ক্লোজার সরাসরি জাভার `java.time.LocalDate` কে স্ট্রিং থেকে তারিখ পার্সিং-এর জন্য ব্যবহার করতে পারে:
```clojure
(require '[clojure.java.io :as io])

; জাভা ইন্টারঅপ ব্যবহার করে তারিখ পার্সিং
(let [date-str "2023-04-01"
      date (java.time.LocalDate/parse date-str)]
  (println date))
; আউটপুট: 2023-04-01
```

### clj-time ব্যবহার করে
তারিখ ও সময়ের সাথে কাজ করার জন্য আরও অভ্যন্তরীন ক্লোজার লাইব্রেরি হল `clj-time`। এটি Joda-Time-কে আবৃত করে, যা একটি বিস্তারিত লাইব্রেরি তারিখ ও সময়ের অপারেশনের জন্য। প্রথমে, আপনাকে আপনার প্রজেক্টের নির্ভরশীলতায় `clj-time` যোগ করতে হবে। এখানে `clj-time` ব্যবহার করে একটি তারিখ স্ট্রিং পার্স করার উদাহরণ দেওয়া হল:

```clojure
; নিশ্চিত করুন যে [clj-time "0.15.2"] আপনার project.clj-এর :dependencies-এর অধীনে যোগ করা হয়েছে

(require '[clj-time.format :as fmt]
         '[clj-time.core :as time])

; একটি ফরমেটার ডিফাইন করুন
(let [formatter (fmt/formatter "yyyy-MM-dd")
      date-str "2023-04-01"
      parsed-date (fmt/parse formatter date-str)]
  (println parsed-date))
; আউটপুট: #object[org.joda.time.DateTime 0x76eccb5d "2023-04-01T00:00:00.000Z"]
```

এই উদাহরণগুলি মৌলিক তারিখ পার্সিং দেখায়। উভয় পদ্ধতিই উপকারী, কিন্তু `clj-time` জটিল প্রয়োজনগুলির জন্য অতিরিক্ত কার্যকারিতা সহ আরও ক্লোজার-কেন্দ্রিক পদ্ধতি প্রদান করতে পারে।

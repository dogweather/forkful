---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:07:07.837774-06:00
description: "\u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F\
  \ \u09AA\u09CD\u09B0\u09BF\u09A8\u09CD\u099F\u09BF\u0982 \u0986\u09AA\u09A8\u09BE\
  \u09B0 \u0995\u09CB\u09A1\u0995\u09C7 \u09AE\u09BE\u099D\u09C7 \u09AE\u09BE\u099D\
  \u09C7 \u09AC\u09CD\u09B0\u09C7\u09A1\u0995\u09CD\u09B0\u09BE\u09AE\u09CD\u09AC\u09B8\
  \u09C7\u09B0 \u09AE\u09A4 \u0995\u09B0\u09C7 \u09A4\u09CB\u09B2\u09C7: \u098F\u099F\
  \u09BF \u09A1\u09C7\u099F\u09BE \u098F\u09AC\u0982 \u09B2\u099C\u09BF\u0995 \u09AB\
  \u09CD\u09B2\u09CB\u09AF\u09BC\u09C7\u09B0 \u09AA\u09A5 \u09A6\u09C7\u0996\u09BE\
  \u09AF\u09BC \u09AF\u0996\u09A8 \u0995\u09CB\u09A1\u099F\u09BF \u0995\u09BE\u09B0\
  \u09CD\u09AF\u0995\u09B0 \u09B9\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF\u2026"
lastmod: '2024-03-17T18:47:43.624071-06:00'
model: gpt-4-0125-preview
summary: "\u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AA\
  \u09CD\u09B0\u09BF\u09A8\u09CD\u099F\u09BF\u0982 \u0986\u09AA\u09A8\u09BE\u09B0\
  \ \u0995\u09CB\u09A1\u0995\u09C7 \u09AE\u09BE\u099D\u09C7 \u09AE\u09BE\u099D\u09C7\
  \ \u09AC\u09CD\u09B0\u09C7\u09A1\u0995\u09CD\u09B0\u09BE\u09AE\u09CD\u09AC\u09B8\
  \u09C7\u09B0 \u09AE\u09A4 \u0995\u09B0\u09C7 \u09A4\u09CB\u09B2\u09C7: \u098F\u099F\
  \u09BF \u09A1\u09C7\u099F\u09BE \u098F\u09AC\u0982 \u09B2\u099C\u09BF\u0995 \u09AB\
  \u09CD\u09B2\u09CB\u09AF\u09BC\u09C7\u09B0 \u09AA\u09A5 \u09A6\u09C7\u0996\u09BE\
  \u09AF\u09BC \u09AF\u0996\u09A8 \u0995\u09CB\u09A1\u099F\u09BF \u0995\u09BE\u09B0\
  \u09CD\u09AF\u0995\u09B0 \u09B9\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF\u2026"
title: "\u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AA\
  \u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09BE"
weight: 33
---

## কী এবং কেন?
ডিবাগ আউটপুট প্রিন্টিং আপনার কোডকে মাঝে মাঝে ব্রেডক্রাম্বসের মত করে তোলে: এটি ডেটা এবং লজিক ফ্লোয়ের পথ দেখায় যখন কোডটি কার্যকর হয়। প্রোগ্রামাররা এটি ব্যবহার করে বিরক্তিকর বাগগুলো খুঁজে বের করতে ও তাদের কোড প্রত্যাশামাফিক আচরণ করছে কিনা তা বুঝতে।

## কিভাবে:
Clojure-এ, প্রায়শই `println`, `printf`, `pr`, অথবা `prn` ব্যবহার করে ডিবাগ আউটপুট প্রিন্ট করা হয়। এখানে কিছু ডিবাগ প্রিন্টস কিভাবে যোগ করবেন তার উদাহরণ দেওয়া হল:

```Clojure
(defn add-and-print [a b]
  (println "Adding:" a "and" b) ; অপারেশনটি প্রিন্ট করে
  (let [result (+ a b)]
    (println "Result:" result)  ; রেজাল্টটি প্রিন্ট করে
    result))                    ; রেজাল্ট রিটার্ন করে

(add-and-print 3 4)
```
নমুনা আউটপুট:
```
Adding: 3 and 4
Result: 7
```

অথবা, একটি থ্রেডিং ম্যাক্রোর মাঝখানে মান ডিবাগ করতে:

```Clojure
(require '[clojure.pprint :refer [pprint]])

(-> 3
    (+ 5)
    (pprint)             ; মধ্যবর্তী রেজাল্ট প্রিন্ট করে
    (* 2))
```
নমুনা আউটপুট:
```
8
```

## গভীর ডুব:
প্রিন্ট ডিবাগিং এর একটি দীর্ঘ ইতিহাস আছে, সম্ভবত প্রোগ্রামিং নিজেই যখন থেকে আরম্ভ হয়েছে। এটি একটি সোজা প্রক্রিয়া: আপনি যেখানে সমস্যা থাকতে পারে সন্দেহ করেন, সেখানে প্রিন্ট স্টেটমেন্টস সংযোজন করেন, কোড চালান, এবং আউটপুট দেখেন।

Clojure-এর ডিবাগ প্রিন্টিং এর ফাংশনগুলো অন্যান্য Lisp ভাষার সাথে বেশ মিলে যায়, তবে সাধারণ ফাংশনাল স্বাদের সাথে। `println` এবং `prn` এর মধ্যে পার্থক্য হল `prn` ডেটা এমনভাবে লিখে যা Clojure রিডার দ্বারা পড়া যায়। `clojure.pprint` থেকে `pprint` (প্রিটি প্রিন্ট) ব্যবহৃত হয় যখন আপনি আরও উন্নত ফরম্যাট চান।

Clojure-এর জন্য একটি নির্দিষ্ট ডিবাগিং টূল হল `tap>`. Clojure 1.10-এ চালু করা, এটি কোডে প্রিন্ট স্টেটমেন্ট ছাড়াই চলমান কোডে 'ট্যাপ' করার অনবরত সুযোগ দেয়।

বৃহত্তর বা জটিল প্রকল্পের জন্য, `clojure.tools.logging` বা `timbre` এর মতো লগিং লাইব্রেরি বিবেচনা করুন।

## দেখুন আরও:
- [`clojure.tools.logging`](https://github.com/clojure/tools.logging) GitHub রিপোজিটরি
- [Timbre লগিং লাইব্রেরি](https://github.com/ptaoussanis/timbre) GitHub রিপোজিটরি
- [`clojure.pprint`](https://clojuredocs.org/clojure.pprint/pprint) ClojureDocs এ নথি

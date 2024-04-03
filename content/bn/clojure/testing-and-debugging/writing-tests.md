---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:40:40.922376-06:00
description: "Clojure \u098F \u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09C7\u0996\u09BE\
  , \u0985\u09A8\u09CD\u09AF \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09BF\u0982 \u09AD\u09BE\u09B7\u09BE\u0997\u09C1\u09B2\u09BF\u09A4\u09C7 \u09B2\
  \u09C7\u0996\u09BE\u09B0 \u09AE\u09A4\u09CB, \u0986\u09AA\u09A8\u09BE\u09B0 \u09AA\
  \u09CD\u09B0\u09A7\u09BE\u09A8 \u0995\u09CB\u09A1\u09AC\u09C7\u09B8 \u09AF\u09C7\
  \u09AE\u09A8\u099F\u09BF \u09AA\u09CD\u09B0\u09A4\u09CD\u09AF\u09BE\u09B6\u09BE\
  \ \u0995\u09B0\u09BE \u09B9\u09AF\u09BC \u09A4\u09C7\u09AE\u09A8\u09BF \u0995\u09BE\
  \u099C \u0995\u09B0\u09C7 \u098F\u09AE\u09A8 \u09A8\u09BF\u09B6\u09CD\u099A\u09BF\
  \u09A4 \u0995\u09B0\u09A4\u09C7 \u0989\u09CE\u09B8\u09B0\u09CD\u0997\u09C0\u0995\
  \u09C3\u09A4 \u0995\u09CB\u09A1\u2026"
lastmod: '2024-03-17T18:47:43.625077-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u098F \u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09C7\u0996\u09BE,\
  \ \u0985\u09A8\u09CD\u09AF \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09BF\u0982 \u09AD\u09BE\u09B7\u09BE\u0997\u09C1\u09B2\u09BF\u09A4\u09C7 \u09B2\
  \u09C7\u0996\u09BE\u09B0 \u09AE\u09A4\u09CB, \u0986\u09AA\u09A8\u09BE\u09B0 \u09AA\
  \u09CD\u09B0\u09A7\u09BE\u09A8 \u0995\u09CB\u09A1\u09AC\u09C7\u09B8 \u09AF\u09C7\
  \u09AE\u09A8\u099F\u09BF \u09AA\u09CD\u09B0\u09A4\u09CD\u09AF\u09BE\u09B6\u09BE\
  \ \u0995\u09B0\u09BE \u09B9\u09AF\u09BC \u09A4\u09C7\u09AE\u09A8\u09BF \u0995\u09BE\
  \u099C \u0995\u09B0\u09C7 \u098F\u09AE\u09A8 \u09A8\u09BF\u09B6\u09CD\u099A\u09BF\
  \u09A4 \u0995\u09B0\u09A4\u09C7 \u0989\u09CE\u09B8\u09B0\u09CD\u0997\u09C0\u0995\
  \u09C3\u09A4 \u0995\u09CB\u09A1 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE \u099C\
  \u09A1\u09BC\u09BF\u09A4\u0964 \u098F\u099F\u09BF \u09B8\u09A0\u09BF\u0995\u09A4\
  \u09BE \u09A8\u09BF\u09B6\u09CD\u099A\u09BF\u09A4 \u0995\u09B0\u09BE, \u09B0\u09BF\
  \u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0\u09BF\u0982 \u09B8\u09B9\u099C\
  \ \u0995\u09B0\u09BE, \u098F\u09AC\u0982 \u0995\u09CB\u09A1 \u09B8\u09CD\u09A5\u09BF\
  \u09A4\u09BF\u09B6\u09C0\u09B2\u09A4\u09BE \u09AC\u09BE\u09A1\u09BC\u09BE\u09A4\u09C7\
  \ \u09B8\u09B9\u09BE\u09AF\u09BC\u0995\u0964."
title: "\u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09BF\u0996\u09BE"
weight: 36
---

## কি এবং কেন?
Clojure এ টেস্ট লেখা, অন্য প্রোগ্রামিং ভাষাগুলিতে লেখার মতো, আপনার প্রধান কোডবেস যেমনটি প্রত্যাশা করা হয় তেমনি কাজ করে এমন নিশ্চিত করতে উৎসর্গীকৃত কোড তৈরি করা জড়িত। এটি সঠিকতা নিশ্চিত করা, রিফ্যাক্টরিং সহজ করা, এবং কোড স্থিতিশীলতা বাড়াতে সহায়ক।

## কিভাবে:
Clojure, JVM কে কাজে লাগিয়ে, বিভিন্ন টেস্টিং ফ্রেমওয়ার্ক সাপোর্ট করে। তবে, একটি সাধারণভাবে ব্যবহৃত বিল্ট-ইন লাইব্রেরি হল `clojure.test`। এখানে একটি সাধারণ উদাহরণ দেওয়া হলঃ

```clojure
(ns example.test
  (:require [clojure.test :refer :all]
            [example.core :refer :all]))

(deftest test-addition
  (testing "Addition functionality"
    (is (= 4 (add 2 2)))
    (is (= 7 (add 3 4)))))

(run-tests)
```
এই টেস্টটি চালানোর পর, আপনি একটি আউটপুট দেখতে পাবেন যা অনুরূপ:

```
Testing example.test

Ran 2 tests containing 2 assertions.
0 failures, 0 errors.
```

যারা আরো বৈশিষ্ট্য-সমৃদ্ধ বিকল্প খোঁজেন, তারা `Midje` অথবা `test.check` এর মতো তৃতীয়-পক্ষের লাইব্রেরিগুলি ব্যবহার করতে পারেন। এখানে আপনি কিভাবে Midje ব্যবহার করে একইরকম টেস্ট করতে পারেন:

প্রথমে, আপনার project.clj নির্ভরতাগুলিতে Midje যোগ করুন:
```clojure
[midje "1.9.9"]
```

তারপর, আপনার Midje দিয়ে টেস্টটি এরকম দেখাবে:

```clojure
(ns example.test
  (:require [midje.sweet :refer :all]
            [example.core :refer :all]))

(fact "Testing addition"
  (add 2 2) => 4
  (add 3 4) => 7)
```

`lein midje` এর মাধ্যমে টেস্টটি চালানোর পর, আউটপুট এমন কিছু প্রদর্শিত হবে:

```
All checks (2) succeeded.
```

---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:40:40.922376-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Clojure, JVM \u0995\u09C7 \u0995\
  \u09BE\u099C\u09C7 \u09B2\u09BE\u0997\u09BF\u09AF\u09BC\u09C7, \u09AC\u09BF\u09AD\
  \u09BF\u09A8\u09CD\u09A8 \u099F\u09C7\u09B8\u09CD\u099F\u09BF\u0982 \u09AB\u09CD\
  \u09B0\u09C7\u09AE\u0993\u09AF\u09BC\u09BE\u09B0\u09CD\u0995 \u09B8\u09BE\u09AA\u09CB\
  \u09B0\u09CD\u099F \u0995\u09B0\u09C7\u0964 \u09A4\u09AC\u09C7, \u098F\u0995\u099F\
  \u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\u09AD\u09BE\u09AC\u09C7 \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09C3\u09A4 \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u09B2\
  \u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09B9\u09B2 `clojure.test`\u0964\
  \ \u098F\u0996\u09BE\u09A8\u09C7\u2026"
lastmod: '2024-03-17T18:47:43.625077-06:00'
model: gpt-4-0125-preview
summary: "Clojure, JVM \u0995\u09C7 \u0995\u09BE\u099C\u09C7 \u09B2\u09BE\u0997\u09BF\
  \u09AF\u09BC\u09C7, \u09AC\u09BF\u09AD\u09BF\u09A8\u09CD\u09A8 \u099F\u09C7\u09B8\
  \u09CD\u099F\u09BF\u0982 \u09AB\u09CD\u09B0\u09C7\u09AE\u0993\u09AF\u09BC\u09BE\u09B0\
  \u09CD\u0995 \u09B8\u09BE\u09AA\u09CB\u09B0\u09CD\u099F \u0995\u09B0\u09C7\u0964\
  \ \u09A4\u09AC\u09C7, \u098F\u0995\u099F\u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\
  \u09AD\u09BE\u09AC\u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\u09C3\u09A4 \u09AC\u09BF\
  \u09B2\u09CD\u099F-\u0987\u09A8 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\
  \u09BF \u09B9\u09B2 `clojure.test`\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\
  \u099F\u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3 \u0989\u09A6\u09BE\u09B9\u09B0\
  \u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2\u0983."
title: "\u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09BF\u0996\u09BE"
weight: 36
---

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

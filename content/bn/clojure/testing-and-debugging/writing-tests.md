---
title:                "টেস্ট লিখা"
date:                  2024-03-17T18:40:40.922376-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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

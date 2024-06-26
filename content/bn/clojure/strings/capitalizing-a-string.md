---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:41.799754-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u0995\u09CD\u09B2\u09CB\u099C\
  \u09BE\u09B0, \u098F\u0995\u099F\u09BF JVM \u09AD\u09BE\u09B7\u09BE \u09B9\u09BF\
  \u09B8\u09C7\u09AC\u09C7, \u0986\u09AA\u09A8\u09BE\u0995\u09C7 \u09B8\u09B0\u09BE\
  \u09B8\u09B0\u09BF Java String \u09AE\u09C7\u09A5\u09A1\u0997\u09C1\u09B2\u09BF\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BE\u09B0 \u0985\u09A8\
  \u09C1\u09AE\u09A4\u09BF \u09A6\u09C7\u09AF\u09BC\u0964 \u098F\u0996\u09BE\u09A8\
  \u09C7 \u0995\u09CD\u09B2\u09CB\u099C\u09BE\u09B0\u09C7 \u098F\u0995\u099F\u09BF\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0995\u09C7\u09AA\u09BF\u099F\u09BE\
  \u09B2\u09BE\u0987\u099C \u0995\u09B0\u09BE\u09B0 \u098F\u0995\u099F\u09BF \u09AC\
  \u09C7\u09B8\u09BF\u0995\u2026"
lastmod: '2024-03-17T18:47:43.602130-06:00'
model: gpt-4-0125-preview
summary: "\u0995\u09CD\u09B2\u09CB\u099C\u09BE\u09B0, \u098F\u0995\u099F\u09BF JVM\
  \ \u09AD\u09BE\u09B7\u09BE \u09B9\u09BF\u09B8\u09C7\u09AC\u09C7, \u0986\u09AA\u09A8\
  \u09BE\u0995\u09C7 \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF Java String \u09AE\u09C7\
  \u09A5\u09A1\u0997\u09C1\u09B2\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09BE\u09B0 \u0985\u09A8\u09C1\u09AE\u09A4\u09BF \u09A6\u09C7\u09AF\
  \u09BC\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u0995\u09CD\u09B2\u09CB\u099C\u09BE\
  \u09B0\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \ \u0995\u09C7\u09AA\u09BF\u099F\u09BE\u09B2\u09BE\u0987\u099C \u0995\u09B0\u09BE\
  \u09B0 \u098F\u0995\u099F\u09BF \u09AC\u09C7\u09B8\u09BF\u0995 \u0989\u09A6\u09BE\
  \u09B9\u09B0\u09A3 \u09B0\u09AF\u09BC\u09C7\u099B\u09C7."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09AA\u09CD\u09B0\
  \u09A5\u09AE \u0985\u0995\u09CD\u09B7\u09B0 \u09AC\u09A1\u09BC \u09B9\u09BE\u09A4\
  \u09C7\u09B0 \u0995\u09B0\u09BE"
weight: 2
---

## কিভাবে:
ক্লোজার, একটি JVM ভাষা হিসেবে, আপনাকে সরাসরি Java String মেথডগুলি ব্যবহার করার অনুমতি দেয়। এখানে ক্লোজারে একটি স্ট্রিং কেপিটালাইজ করার একটি বেসিক উদাহরণ রয়েছে:

```clojure
(defn capitalize-string [s]
  (if (empty? s)
    s
    (str (clojure.string/upper-case (subs s 0 1)) (subs s 1))))

(capitalize-string "hello world!") ; => "Hello world!"
```

ক্লোজারে স্পেসিফিকভাবে স্ট্রিং কেপিটালাইজ করার জন্য কোন বিল্ট-ইন ফাংশন অন্তর্ভুক্ত নেই, তবে দেখানো হিসেবে, `clojure.string/upper-case`, `subs`, এবং `str` ফাংশনগুলি সম্মিলন করে আপনি সহজেই এটি অর্জন করতে পারেন।

আরও সংক্ষিপ্ত সমাধানের জন্য এবং আরও জটিল স্ট্রিং ম্যানিপুলেশনগুলি সমাধানের জন্য, আপনি একটি থার্ড-পার্টি লাইব্রেরির দিকে মোড় নিতে পারেন। ক্লোজার ইকোসিস্টেমে এমন একটি জনপ্রিয় লাইব্রেরি হচ্ছে `clojure.string`। তবে আমার শেষ আপডেট অনুসারে, এটি মূল ক্লোজার ফাংশনালিটিগুলি দিয়ে দেখানো ছাড়া ক্যাপিটালাইজ ফাংশনের জন্য সরাসরি কোন অফার প্রদান করে না, তাই উপরে দেখানো পদ্ধতিটিই হল ক্যাপিটালাইজেশনের জন্য অতিরিক্ত লাইব্রেরি না টানা সরাসরি উপস্থাপন করা আপনার কাছে।

মনে রাখবেন, ক্লোজারে স্ট্রিং দিয়ে কাজ করার সময় যা জাভা মেথডগুলির সাথে মিথস্ক্রিয়া করে, আপনি অবশ্যই জাভা স্ট্রিংগুলি দিয়ে কাজ করছেন, যা আপনাকে আপনার ক্লোজার কোডে সরাসরি জাভা এর সমস্ত স্ট্রিং মেথডগুলি ব্যবহার করে সুবিধা দেয়।

---
title:                "স্ট্রিং ইন্টারপোলেট করা"
date:                  2024-03-17T17:50:41.727445-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি ও কেন?
স্ট্রিং ইন্টারপোলেশন আমাদেরকে স্ট্রিং এর মধ্যে ভেরিয়েবল ঢুকাতে দেয় বিরক্তি ছাড়াই। কেন? ডায়নামিকালি টেক্সট নির্মাণ করার জন্য—যা পুরোনো স্কুলের স্ট্রিং যোগ করার চেয়ে অনেক সুবিধাজনক।

## কিভাবে:
```Clojure
;; `str` এবং `format` ব্যবহার করে মৌলিক
(def name "World")
(str "Hello, " name "!")  ; => "Hello, World!"

;; `format` ব্যবহার করে, printf-শৈলী ফরম্যাটিং এর অনুরূপ
(format "Goodbye, %s!" name)  ; => "Goodbye, World!"

;; Clojure এ অন্যান্য ভাষার মতো বিল্ট-ইন স্ট্রিং ইন্টারপোলেশন নেই,
;; কিন্তু আমরা `str` এবং `format` এর সাথে সৃজনশীল হতে পারি।
```

## গভীরে ডাইভ:
Clojure একটু ত্যাগী: কোনো বিল্ট-ইন স্ট্রিং ইন্টারপোলেশন নেই। তবে, ডায়নামিক স্ট্রিং-এর জন্য `str` এবং `format` হল প্রথম পছন্দ। মূল কাহিনী? Clojure-এর সহজাত দর্শন। এটি বিশ্বাস করে আমরা নিজেরা স্ট্রিং নির্মাণ করতে পারি।

বিকল্পের জন্যে, টেমপ্লেটিং বিশ্বে প্রবেশ করুন: `clostache` (Mustache-এর একটি Clojure বাস্তবায়ন) অথবা HTML প্রেক্ষাপটে `hiccup`। যখন `str` এবং `format` খুব প্রাথমিক মনে হয়, তখন এগুলো কাজে আসে।

অন্তরালে, `format` Java-এর `String.format`-এ নির্ভর করে, একটি তথ্য যা Clojure-এর জাভা ইন্টারপারাবিলিটি সুপারপাওয়ার প্রদর্শন করে। তাই, আপনি মিষ্টি পেলেন না হলেও, যখন প্রয়োজন হয় তখন আপনার কাছে জাভা-এর শক্তি আছে।

## দেখুন ও:
- `str` সম্পর্কে Clojure ডকস: https://clojuredocs.org/clojure.core/str
- `format` সম্পর্কে Clojure ডকস: https://clojuredocs.org/clojure.core/format
- clostache GitHub repo: https://github.com/fhd/clostache
- hiccup GitHub repo: https://github.com/weavejester/hiccup

---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:26:21.140708-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B2\u09BF\u09B8\u09CD\u09AA\
  \ \u09AA\u09B0\u09BF\u09AC\u09BE\u09B0\u09C7 \u09A4\u09BE\u09B0 \u09B6\u09C7\u0995\
  \u09DC \u09AC\u09BF\u099A\u09CD\u099B\u09BF\u09A8\u09CD\u09A8 \u09A8\u09BE \u0995\
  \u09B0\u09C7, \u0995\u09CD\u09B2\u09CB\u099C\u09BE\u09B0 \u099C\u09BE\u09AD\u09BE\
  \u09B0 \u09B0\u09C7\u0997\u09C1\u09B2\u09BE\u09B0 \u098F\u0995\u09CD\u09B8\u09AA\
  \u09CD\u09B0\u09C7\u09B6\u09A8 \u0995\u09CD\u09B7\u09AE\u09A4\u09BE\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u0985\u09AC\u09BF\u099A\u09CD\u099B\u09C7\u09A6\u09CD\u09AF\u09AD\
  \u09BE\u09AC\u09C7 \u09AE\u09C7\u09B2\u09BE\u09A8\u09CB \u098F\u0995 \u09A7\u09A8\
  \u09C0 \u09AB\u09BE\u0982\u09B6\u09A8 \u09B8\u09C7\u099F \u0985\u09AB\u09BE\u09B0\
  \ \u0995\u09B0\u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7\u2026"
lastmod: '2024-03-17T18:47:43.610055-06:00'
model: gpt-4-0125-preview
summary: "\u09B2\u09BF\u09B8\u09CD\u09AA \u09AA\u09B0\u09BF\u09AC\u09BE\u09B0\u09C7\
  \ \u09A4\u09BE\u09B0 \u09B6\u09C7\u0995\u09DC \u09AC\u09BF\u099A\u09CD\u099B\u09BF\
  \u09A8\u09CD\u09A8 \u09A8\u09BE \u0995\u09B0\u09C7, \u0995\u09CD\u09B2\u09CB\u099C\
  \u09BE\u09B0 \u099C\u09BE\u09AD\u09BE\u09B0 \u09B0\u09C7\u0997\u09C1\u09B2\u09BE\
  \u09B0 \u098F\u0995\u09CD\u09B8\u09AA\u09CD\u09B0\u09C7\u09B6\u09A8 \u0995\u09CD\
  \u09B7\u09AE\u09A4\u09BE\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0985\u09AC\u09BF\u099A\
  \u09CD\u099B\u09C7\u09A6\u09CD\u09AF\u09AD\u09BE\u09AC\u09C7 \u09AE\u09C7\u09B2\u09BE\
  \u09A8\u09CB \u098F\u0995 \u09A7\u09A8\u09C0 \u09AB\u09BE\u0982\u09B6\u09A8 \u09B8\
  \u09C7\u099F \u0985\u09AB\u09BE\u09B0 \u0995\u09B0\u09C7\u0964 \u098F\u0996\u09BE\
  \u09A8\u09C7 \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u0986\u09AA\u09A8\u09BF \u09A4\
  \u09BE\u09A6\u09C7\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8."
title: "\u09B0\u09C7\u0997\u09C1\u09B2\u09BE\u09B0 \u098F\u0995\u09CD\u09B8\u09AA\u09CD\
  \u09B0\u09C7\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09BE"
weight: 11
---

## কিভাবে:
লিস্প পরিবারে তার শেকড় বিচ্ছিন্ন না করে, ক্লোজার জাভার রেগুলার এক্সপ্রেশন ক্ষমতার সাথে অবিচ্ছেদ্যভাবে মেলানো এক ধনী ফাংশন সেট অফার করে। এখানে কিভাবে আপনি তাদের ব্যবহার করতে পারেন:

### বেসিক ম্যাচিং
একটা স্ট্রিং যদি একটি প্যাটার্নের সাথে মিলে যায় তা পরীক্ষা করার জন্য, `re-matches` ব্যবহার করুন। এটি সফল হলে পুরো মিল ফিরিয়ে দেয় অথবা অন্যথায় `nil` ফিরিয়ে দেয়।

```clojure
(re-matches #"\d+" "123")  ;=> "123"
(re-matches #"\d+" "abc")  ;=> nil
```

### প্যাটার্ন সন্ধান করা
প্যাটার্নের প্রথম উদাহরণ খুঁজে পাওয়ার জন্য, `re-find` হ'ল আপনার সেরা বন্ধু:

```clojure
(re-find #"\d+" "Order 123")  ;=> "123"
```

### গ্রুপ ক্যাপচার করা
আপনার প্যাটার্নে প্যারেনথেসিস ব্যবহার করে গ্রুপ ক্যাপচার করতে `re-find` ব্যবহার করুন:

```clojure
(let [[_ area code] (re-find #"(1)?(\d{3})" "Phone: 123-4567")]
  (println "Area Code:" area "Code:" code))
;; আউটপুট: Area Code: nil Code: 123
```

### গ্লোবাল সার্চ (সমস্ত ম্যাচ খুঁজে পাওয়া)
কিছু ভাষার মতো ক্লোজারের অন্তর্নির্মিত গ্লোবাল সার্চ নেই। বরং, সমস্ত ম্যাচের একটি অলস সিকোয়েন্স পেতে `re-seq` ব্যবহার করুন:

```clojure
(re-seq #"\d+" "id: 123, qty: 456")  ;=> ("123" "456")
```

### স্ট্রিং বিভাজন
একটি স্ট্রিংকে একটি প্যাটার্ন অনুসারে বিভক্ত করতে, `clojure.string/split` ব্যবহার করুন:

```clojure
(clojure.string/split "John,Doe,30" #",")  ;=> ["John" "Doe" "30"]
```

### প্রতিস্থাপন
একটি স্ট্রিংয়ের যে কোনো অংশ যা একটি প্যাটার্নের সাথে মিলে যায় তাকে `clojure.string/replace` দ্বারা প্রতিস্থাপন করুন:

```clojure
(clojure.string/replace "2023-04-01" #"\d{4}" "YYYY")  ;=> "YYYY-04-01"
```

### তৃতীয় পক্ষের লাইব্রেরি
যদিও ক্লোজারের অন্তর্নির্মিত সমর্থন বেশিরভাগ মামলার জন্য যথেষ্ট, আরও জটিল সিনারিওর জন্য, বুদ্ধিমত্তার সাথে ডাটা ভ্যালিডেশনের জন্য `clojure.spec` এবং ওয়েব অ্যাপ্লিকেশনগুলিতে regex-ভিত্তিক রাউটিং এবং ইনপুট ভ্যালিডেশনের সাথে রিঅ্যাকটিভ DOM ম্যানিপুলেশনের জন্য `reagent` এর মতো লাইব্রেরিগুলি ব্যবহার করার কথা ভাবুন।

```clojure
;; একটি ইমেইল বৈধ করে দেখার জন্য clojure.spec ব্যবহারের উদাহরণ
(require '[clojure.spec.alpha :as s])
(s/def ::email (s/and string? #(re-matches #".+@.+\..+" %)))
(s/valid? ::email "test@example.com")  ;=> true
```

মনে রাখবেন, রেগুলার এক্সপ্রেশন যখন শক্তিশালী, তখন তা কোড পড়া এবং রক্ষণাবেক্ষণ করা কঠিন করে তোলে। এগুলি বিবেচনাপূর্বক ব্যবহার করুন এবং সম্ভব হলে সরল স্ট্রিং ম্যানিপুলেশন ফাংশন বিবেচনা করুন।

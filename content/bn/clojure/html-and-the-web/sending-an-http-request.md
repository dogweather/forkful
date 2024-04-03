---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:17:14.116131-06:00
description: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A8\
  \u09CB \u09B9\u099A\u09CD\u099B\u09C7 \u0986\u09AA\u09A8\u09BE\u09B0 \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09C7\u09B0 \u0985\u09A8\u09CD\u09AF\
  \ \u098F\u0995\u099F\u09BF \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7\u09B0\
  \ \u0995\u09BE\u099B \u09A5\u09C7\u0995\u09C7 \u09A1\u09C7\u099F\u09BE \u0985\u09A5\
  \u09AC\u09BE \u09B8\u09C7\u09AC\u09BE \u099A\u09BE\u0993\u09AF\u09BC\u09BE\u09B0\
  \ \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE \u0993\u09AF\u09BC\
  \u09C7\u09AC\u09C7\u09B0 \u0993\u09AA\u09B0 \u09A6\u09BF\u09AF\u09BC\u09C7\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u098F\u099F\u09BE \u0995\u09B0\u09C7\u09A8 \u0993\u09AF\u09BC\u09C7\u09AC\u2026"
lastmod: '2024-03-17T18:47:43.618012-06:00'
model: gpt-4-0125-preview
summary: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A8\
  \u09CB \u09B9\u099A\u09CD\u099B\u09C7 \u0986\u09AA\u09A8\u09BE\u09B0 \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09C7\u09B0 \u0985\u09A8\u09CD\u09AF\
  \ \u098F\u0995\u099F\u09BF \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7\u09B0\
  \ \u0995\u09BE\u099B \u09A5\u09C7\u0995\u09C7 \u09A1\u09C7\u099F\u09BE \u0985\u09A5\
  \u09AC\u09BE \u09B8\u09C7\u09AC\u09BE \u099A\u09BE\u0993\u09AF\u09BC\u09BE\u09B0\
  \ \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE \u0993\u09AF\u09BC\
  \u09C7\u09AC\u09C7\u09B0 \u0993\u09AA\u09B0 \u09A6\u09BF\u09AF\u09BC\u09C7\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u098F\u099F\u09BE \u0995\u09B0\u09C7\u09A8 \u0993\u09AF\u09BC\u09C7\u09AC API-\u098F\
  \u09B0 \u09B8\u0999\u09CD\u0997\u09C7 \u09AE\u09BF\u09A5\u09B8\u09CD\u0995\u09CD\
  \u09B0\u09BF\u09AF\u09BC\u09BE \u0995\u09B0\u09A4\u09C7, \u09B0\u09BF\u09B8\u09CB\
  \u09B0\u09CD\u09B8 \u0986\u09A8\u09A4\u09C7, \u0985\u09A5\u09AC\u09BE \u09B8\u09BE\
  \u09B0\u09CD\u09AD\u09BF\u09B8\u0997\u09C1\u09B2\u09BF\u09B0 \u09AE\u09A7\u09CD\u09AF\
  \u09C7 \u09AF\u09CB\u0997\u09BE\u09AF\u09CB\u0997 \u0995\u09B0\u09A4\u09C7\u0964\
  ."
title: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3\
  \ \u0995\u09B0\u09BE"
weight: 44
---

## কি এবং কেন?
HTTP অনুরোধ পাঠানো হচ্ছে আপনার প্রোগ্রামের অন্য একটি সিস্টেমের কাছ থেকে ডেটা অথবা সেবা চাওয়ার প্রক্রিয়া ওয়েবের ওপর দিয়ে। প্রোগ্রামাররা এটা করেন ওয়েব API-এর সঙ্গে মিথস্ক্রিয়া করতে, রিসোর্স আনতে, অথবা সার্ভিসগুলির মধ্যে যোগাযোগ করতে।

## কিভাবে:
Clojure-এ, আপনি `clj-http` ক্লাইন্ট ব্যবহার করে HTTP অনুরোধ পাঠাতে পারেন।

প্রথমে, আপনার `project.clj`-এ নির্ভরতা যোগ করুন:
```clojure
[clj-http "3.12.3"]
```

এখন, আসুন একটি GET অনুরোধ পাঠাই:
```clojure
(require '[clj-http.client :as client])

(let [response (client/get "http://httpbin.org/get")]
  (println response))
```

আউটপুট নমুনা:
```clojure
{:status 200, :headers {...}, :body "..."}
```

ডেটা পোস্ট করতে:
```clojure
(let [response (client/post "http://httpbin.org/post" {:form-params {:key "value"}})]
  (println response))
```

## গভীর ডাইভ
HTTP অনুরোধ পাঠানো নতুন কিছু নয়। এটি ওয়েবের সাথে প্রায় পুরোনো। Clojure, একটি আধুনিক Lisp হিসেবে, HTTP অনুরোধ করতে বিভিন্ন লাইব্রেরি রয়েছে। `clj-http` একটি জনপ্রিয় লাইব্রেরি, তবে `http-kit` অথবা Clojure-র কোর `clj-http.client` আরও বিদ্যমান।

`clj-http` ভেতরের দিকে Apache HttpComponents Client এর উপর Java-এ ভিত্তি করে তৈরি। এটি বহুমুখী কিন্তু Java-নির্ভর মনে হতে পারে। একটি বিকল্প, `http-kit`, সহজ ও Clojure-উপযোগী কিন্তু কম বৈশিষ্ট্যসম্পন্ন।

যখন আপনি HTTP অনুরোধ পাঠান, আপনি TCP/IP এর উপর দিয়ে তা করেন, যা আপনার অনুরোধকে একটি সুপ্রতিষ্ঠিত প্রটোকল অনুসারে ফ্রেম করে। এই বিশ্বজনীন মানদণ্ড আপনাকে প্রায় যে কোনো ওয়েব সার্ভিসের সাথে মিথস্ক্রিয়া করতে দেয়।

## আরও দেখুন
- `clj-http` GitHub রিপোজিটরি: https://github.com/dakrone/clj-http
- অফিসিয়াল Clojure সাইট: https://clojure.org
- HttpComponents Client ডকুমেন্টেশন: https://hc.apache.org/httpcomponents-client-ga/
- বাস্তব-সময়ের চাহিদার জন্য, `http-kit` বিবেচনা করুন: http://www.http-kit.org

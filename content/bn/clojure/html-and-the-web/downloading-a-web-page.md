---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:27.127694-06:00
description: "\u0993\u09AF\u09BC\u09C7\u09AC \u09AA\u09C7\u099C \u09A1\u09BE\u0989\
  \u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 URL \u09A5\u09C7\
  \u0995\u09C7 HTML \u09A8\u09C7\u0993\u09AF\u09BC\u09BE \u09AF\u09BE\u09A4\u09C7\
  \ \u0986\u09AA\u09A8\u09BE\u09B0 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\
  \u09AE \u09A4\u09BE\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A1\u09BE\u099F\u09BE \u09B8\u09CD\
  \u0995\u09CD\u09B0\u09CD\u09AF\u09BE\u09AA \u0995\u09B0\u09BE, \u0993\u09AF\u09BC\
  \u09C7\u09AC \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u200C\u09CD\u09AF\u09BE\u0995\u09B6\
  \u09A8\u0997\u09C1\u09B2\u09BF\u2026"
lastmod: '2024-03-17T18:47:43.620024-06:00'
model: gpt-4-0125-preview
summary: "\u0993\u09AF\u09BC\u09C7\u09AC \u09AA\u09C7\u099C \u09A1\u09BE\u0989\u09A8\
  \u09B2\u09CB\u09A1 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 URL \u09A5\u09C7\u0995\
  \u09C7 HTML \u09A8\u09C7\u0993\u09AF\u09BC\u09BE \u09AF\u09BE\u09A4\u09C7 \u0986\
  \u09AA\u09A8\u09BE\u09B0 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \ \u09A4\u09BE\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09A4\
  \u09C7 \u09AA\u09BE\u09B0\u09C7\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A1\u09BE\u099F\u09BE \u09B8\u09CD\u0995\
  \u09CD\u09B0\u09CD\u09AF\u09BE\u09AA \u0995\u09B0\u09BE, \u0993\u09AF\u09BC\u09C7\
  \u09AC \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u200C\u09CD\u09AF\u09BE\u0995\u09B6\u09A8\
  \u0997\u09C1\u09B2\u09BF\u2026"
title: "\u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC\u09AA\u09C7\u099C\
  \ \u09A1\u09BE\u0989\u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
ওয়েব পেজ ডাউনলোড করা মানে URL থেকে HTML নেওয়া যাতে আপনার প্রোগ্রাম তার সাথে কাজ করতে পারে। প্রোগ্রামাররা ডাটা স্ক্র্যাপ করা, ওয়েব ইন্টার‌্যাকশনগুলি অটোমেট করা অথবা সাইটের স্ট্যাটাস চেক করার জন্য এটি করে থাকেন।

## কিভাবে:
Clojure এ, আপনি `clj-http` ব্যবহার করে দ্রুতগতিতে ওয়েব পেজ ডাউনলোড করতে পারেন। এখানে একটি মৌলিক উদাহরণ দেওয়া হল:

```Clojure
(require '[clj-http.client :as client])

(defn download-page [url]
  (client/get url))

;; এটি এমনভাবে ব্যবহার করুন:
(defn -main []
  (println (download-page "http://example.com")))
```

যদি আপনি এটি চেষ্টা করেন, আপনি একগুচ্ছ বিস্তারিত তথ্য পাবেন। মূল্যবান তথ্যগুলি `:body` এবং `:status` এর নিচে পাওয়া যায়।

## গভীরে যাওয়া
ঐতিহাসিকভাবে, ওয়েব ডাউনলোড কমান্ড লাইনে 'wget' অথবা 'curl' এর মতো ছিল। এখন, Clojure এর মতো ভাষাগুলি লাইব্রেরিগুলির মাধ্যমে এই প্রক্রিয়াকে সহজ করে তুলেছে। `clj-http` এমন একটি লাইব্রেরি যা Clojure এর ফাংশনাল শৈলীর জন্য Java's Apache HttpComponents কে মোড়ানো।

বিকল্প? নিশ্চিত। আপনি সরাসরি `java.net.HttpURLConnection` বা `http-kit` এর মতো অন্যান্য লাইব্রেরি চয়ন করতে পারেন – কিন্তু `clj-http` সহজ এবং বাক্সের বাইরে প্রায় সবকিছু দিয়ে দেয়।

গঠন এর দিক থেকে, `clj-http` আপনার অনুরোধকে একটি জাভা HTTP এন্টিটিতে রূপান্তর করে, কল করে এবং প্রতিক্রিয়াটি ফেরত পাঠায়। পেছনের দিকে, এটি রিডিরেক্টগুলি হ্যান্ডলিং করছে, হেডারগুলি পার্স করছে, এবং প্রতিক্রিয়া দেহটি পরিচালনা করছে যাতে আপনি আপনার ডাটা নিয়ে মনোনিবেশ করতে পারেন, সাঁজোয়া নয়।

## আরও দেখুন
- clj-http GitHub রেপো: [https://github.com/dakrone/clj-http](https://github.com/dakrone/clj-http)
- অন্য একটি পদ্ধতির জন্য Clojure http-kit: [http://www.http-kit.org](http://www.http-kit.org)
- ভাষা সম্পর্কে আরও জানতে অফিশিয়াল Clojure সাইট: [https://clojure.org](https://clojure.org)

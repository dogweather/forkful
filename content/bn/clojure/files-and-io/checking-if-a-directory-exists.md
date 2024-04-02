---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:35.263295-06:00
description: "Clojure \u098F \u098F\u0995\u099F\u09BF \u09A1\u09BF\u09B0\u09C7\u0995\
  \u09CD\u099F\u09B0\u09BF \u0986\u099B\u09C7 \u0995\u09BF\u09A8\u09BE \u09AA\u09B0\
  \u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\u09A4\u09C7 \u09B9\u09B2\u09C7 \u0986\
  \u09AA\u09A8\u09BE\u09B0 Clojure \u0985\u09CD\u09AF\u09BE\u09AA\u09CD\u09B2\u09BF\
  \u0995\u09C7\u09B6\u09A8 \u09A5\u09C7\u0995\u09C7 \u09AB\u09BE\u0987\u09B2 \u09B8\
  \u09BF\u09B8\u09CD\u099F\u09C7\u09AE \u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\
  \u09BF\u09B0 \u0989\u09AA\u09B8\u09CD\u09A5\u09BF\u09A4\u09BF \u09AF\u09BE\u099A\
  \u09BE\u0987 \u0995\u09B0\u09BE \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8\
  \u0964 \u09AB\u09BE\u0987\u09B2 \u0985\u09AA\u09BE\u09B0\u09C7\u09B6\u09A8\u0997\
  \u09C1\u09B2\u09BF\u09A4\u09C7\u2026"
lastmod: '2024-03-17T18:47:43.636711-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u098F \u098F\u0995\u099F\u09BF \u09A1\u09BF\u09B0\u09C7\u0995\u09CD\
  \u099F\u09B0\u09BF \u0986\u099B\u09C7 \u0995\u09BF\u09A8\u09BE \u09AA\u09B0\u09C0\
  \u0995\u09CD\u09B7\u09BE \u0995\u09B0\u09A4\u09C7 \u09B9\u09B2\u09C7 \u0986\u09AA\
  \u09A8\u09BE\u09B0 Clojure \u0985\u09CD\u09AF\u09BE\u09AA\u09CD\u09B2\u09BF\u0995\
  \u09C7\u09B6\u09A8 \u09A5\u09C7\u0995\u09C7 \u09AB\u09BE\u0987\u09B2 \u09B8\u09BF\
  \u09B8\u09CD\u099F\u09C7\u09AE \u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF\
  \u09B0 \u0989\u09AA\u09B8\u09CD\u09A5\u09BF\u09A4\u09BF \u09AF\u09BE\u099A\u09BE\
  \u0987 \u0995\u09B0\u09BE \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8\u0964\
  \ \u09AB\u09BE\u0987\u09B2 \u0985\u09AA\u09BE\u09B0\u09C7\u09B6\u09A8\u0997\u09C1\
  \u09B2\u09BF\u09A4\u09C7\u2026"
title: "\u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF \u0986\u099B\u09C7\
  \ \u0995\u09BF\u09A8\u09BE \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\
  \u09BE"
weight: 20
---

## কি এবং কেন?
Clojure এ একটি ডিরেক্টরি আছে কিনা পরীক্ষা করতে হলে আপনার Clojure অ্যাপ্লিকেশন থেকে ফাইল সিস্টেম ডিরেক্টরির উপস্থিতি যাচাই করা প্রয়োজন। ফাইল অপারেশনগুলিতে এই কাজটি অত্যন্ত গুরুত্বপূর্ণ, যাতে না থাকার সূচনা স্থানান্তর বা লেখার সময় ত্রুটি হয় না, এবং এটি নিশ্চিত করে যে কোড নির্ভুল এবং ত্রুটিমুক্তভাবে নিষ্পাদন হয়।

## কিভাবে:
Clojure, একটি JVM ভাষা হিসাবে, এই উদ্দেশ্যে Java-র `java.io.File` ক্লাস ব্যবহার করতে পারে। এমন একটি মৌলিক অপারেশনের জন্য আপনার কোনো তৃতীয় পক্ষের লাইব্রেরির প্রয়োজন নেই। আপনি এটি কিভাবে করতে পারেন তা নিচে দেখানো হল:

```clojure
(ইমপোর্ট 'java.io.File)

(defn directory-exists? [dir-path]
  (let [dir (File. dir-path)]
    (.exists dir)))

;; ব্যবহারের উদাহরণ
(println (directory-exists? "/path/to/your/directory")) ;; true অথবা false
```

`directory-exists?` এই ফাংশনটি একটি ডিরেক্টরির পথকে স্ট্রিং হিসেবে নেয় এবং যদি ডিরেক্টরি বিদ্যমান থাকে তবে `true` এবং অন্যথায় `false` ফেরত দেয়। এটি ডিরেক্টরি পথের সাথে একটি `File` অবজেক্ট তৈরি করে এবং তারপর এই অবজেক্টের উপর `.exists` মেথড কল করে অর্জিত হয়।

কাঁচা Java ইন্টারপের পাশাপাশি, আপনি এমন Clojure লাইব্রেরি ব্যবহার করতে পারেন যা কিছু Java বয়লারপ্লেট লুকিয়ে রাখে। এমন একটি লাইব্রেরি হচ্ছে `clojure.java.io`। তবে, একটি ডিরেক্টরি আছে কিনা তা যাচাই করার জন্য, আপনি এখনও `File` ক্লাস ব্যবহার করবেন, তবে আপনি অন্যান্য ফাইল অপারেশনের জন্য এই লাইব্রেরি উপকারী পাবেন। উদাহরণ:

```clojure
(require '[clojure.java.io :as io])

(defn directory-exists?-clojure [dir-path]
  (.exists (io/file dir-path)))

;; উদাহরণের ব্যবহার
(println (directory-exists?-clojure "/another/path/to/check")) ;; true অথবা false
```

এই সংস্করণটি খুবই অনুরূপ, তবে Clojure-র `io/file` ফাংশনটি ব্যবহার করে `File` অবজেক্ট তৈরি করে। এই পদ্ধতি Java ক্লাসগুলির সাথে সরাসরি ইন্টারফেসিং এর পরিবর্তে, IO অপারেশনের জন্য Clojure-র লাইব্রেরি ব্যবহার করে Clojure কোডবেসগুলিতে আরও স্বাভাবিকভাবে মিশে যায়।

---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:05.203487-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Clojure \u098F\u099F\u09BF \u09B8\
  \u09B9\u099C \u0995\u09B0\u09C7 \u09A6\u09C7\u09AF\u09BC\u0964 `clojure.java.io`\
  \ \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u0986\u09AA\u09A8\u09BE\
  \u09B0 \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u09AE\u09C7\u099F\u09BE\
  \u09AF\u09BC\u0964."
lastmod: '2024-03-17T18:47:43.642686-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u098F\u099F\u09BF \u09B8\u09B9\u099C \u0995\u09B0\u09C7 \u09A6\u09C7\
  \u09AF\u09BC\u0964 `clojure.java.io` \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\
  \u09BF \u0986\u09AA\u09A8\u09BE\u09B0 \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\
  \u09A8 \u09AE\u09C7\u099F\u09BE\u09AF\u09BC\u0964."
title: "\u098F\u0995\u099F\u09BF \u0985\u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0\
  \ \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE"
weight: 21
---

## কিভাবে:
Clojure এটি সহজ করে দেয়। `clojure.java.io` লাইব্রেরি আপনার প্রয়োজন মেটায়।

```Clojure
(require '[clojure.java.io :as io])

; একটি অস্থায়ী ফাইল তৈরি করুন
(def temp-file (io/file (io/create-temp-file "prefix-" ".txt")))

; অস্থায়ী ফাইল ব্যবহার করুন
(spit temp-file "অস্থায়ী ডেটা অস্থায়ী")

; বিষয়বস্তু পরীক্ষা করুন
(println (slurp temp-file)) ; => "অস্থায়ী ডেটা অস্থায়ী"

; আপনি যখন শেষ করবেন, তখন অস্থায়ী ফাইল মুছে ফেলুন
(.delete temp-file)
```

কিছুই চিরস্থায়ী নয়। আমাদের অস্থায়ী ডেটা এখন শান্তিতে বিশ্রাম নিচ্ছে।

## গভীর ডাইভ
কম্পিউটিংয়ের প্রাথমিক দিনগুলো থেকেই অস্থায়ী ফাইলের ধারণা ছিল, মূলত সীমিত প্রাথমিক সংরক্ষণ ব্যবহার এড়াতে। এটি যেন ডিজিটাল ভাড়া-জায়গা।

Clojure এখানে Java-র উপর ভরসা করে, Java-র `File` ক্লাসের সুবিধাগুলিকে ব্যবহার করে। যদিও আপনি সরাসরি Java জঙ্গলে ডাইভ করতে পারেন, Clojure এটিকে ঝাঁপসা মোড়া দিয়েছে।

বিকল্প? অবশ্যই। অস্থায়ী ডিরেক্টরি এক বিষয়। কিন্তু সে এক আলাদা গল্প, এবং Clojure এটিও কভার করেছে (`create-temp-dir` দেখুন)।

সরাসরি মেমরি ব্যবহার করা হয় না কেন? ভালো, অস্থায়ী ফাইলগুলি RAM-এ সঞ্চয় করার মতো বড় ডেটার জন্য আদর্শ,অথবা যখন আপনি দীর্ঘ মেয়াদে সংরক্ষণ বা পরিষ্কার করা নিয়ে চিন্তা না করে একটি বাস্তব ফাইল চান।

## দেখুন সাথে
- Clojure-র নিজস্ব [IO ডকুমেন্টেশন](https://clojure.github.io/clojure/clojure.java.io-api.html)
- Java-র [File ডকস](https://docs.oracle.com/javase/7/docs/api/java/io/File.html) — ভিত্তির বিস্তারিত জন্য।
- বড় মাপের এবং বেসিকের বাইরে আরও জটিল ফাইল অপারেশনের জন্য হয়তো [Java's NIO ফাইল প্যাকেজ](https://docs.oracle.com/javase/8/docs/api/java/nio/file/package-summary.html) ঘুরে দেখুন।

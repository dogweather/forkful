---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:41.439596-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Clojure \u09AB\u09BE\u0982\u09B6\
  \u09A8\u0997\u09C1\u09B2\u09BF `defn` \u09A6\u09BF\u09AF\u09BC\u09C7 \u09B8\u0982\
  \u099C\u09CD\u099E\u09BE\u09AF\u09BC\u09BF\u09A4 \u09B9\u09AF\u09BC, \u098F\u09B0\
  \ \u09AA\u09B0 \u09A8\u09BE\u09AE, \u09AA\u09CD\u09AF\u09BE\u09B0\u09BE\u09AE\u09BF\
  \u099F\u09BE\u09B0 \u098F\u09AC\u0982 \u09AC\u09A1\u09BF \u0985\u09A8\u09C1\u09B8\
  \u09B0\u09A3 \u0995\u09B0\u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\
  \u099F\u09BF \u09A6\u09CD\u09B0\u09C1\u09A4 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3\
  \ \u09A6\u09C7\u0996\u09C1\u09A8\u0964."
lastmod: '2024-03-17T18:47:43.627087-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u09AB\u09BE\u0982\u09B6\u09A8\u0997\u09C1\u09B2\u09BF `defn` \u09A6\
  \u09BF\u09AF\u09BC\u09C7 \u09B8\u0982\u099C\u09CD\u099E\u09BE\u09AF\u09BC\u09BF\u09A4\
  \ \u09B9\u09AF\u09BC, \u098F\u09B0 \u09AA\u09B0 \u09A8\u09BE\u09AE, \u09AA\u09CD\
  \u09AF\u09BE\u09B0\u09BE\u09AE\u09BF\u099F\u09BE\u09B0 \u098F\u09AC\u0982 \u09AC\
  \u09A1\u09BF \u0985\u09A8\u09C1\u09B8\u09B0\u09A3 \u0995\u09B0\u09C7\u0964 \u098F\
  \u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09A6\u09CD\u09B0\u09C1\u09A4\
  \ \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0996\u09C1\u09A8\u0964."
title: "\u0995\u09CB\u09A1\u0995\u09C7 \u09AB\u09BE\u0982\u09B6\u09A8\u0997\u09C1\u09B2\
  \u09BF\u09A4\u09C7 \u0986\u09AF\u09BC\u09CB\u099C\u09A8 \u0995\u09B0\u09BE"
weight: 18
---

## কিভাবে:
Clojure ফাংশনগুলি `defn` দিয়ে সংজ্ঞায়িত হয়, এর পর নাম, প্যারামিটার এবং বডি অনুসরণ করে। এখানে একটি দ্রুত উদাহরণ দেখুন।

```Clojure
(defn greet [name]
  (str "Hello, " name "!"))

(greet "Alex") ; => "Hello, Alex!"
```

এখন ধরুন আমরা একটি আয়তক্ষেত্রের ক্ষেত্রফল নির্ণয় করতে চাই। সবকিছু একসাথে জড়ো করে ফেলার পরিবর্তে, আমরা এটি দুটি ফাংশনে পৃথক করি:

```Clojure
(defn area [length width]
  (* length width))

(defn print-area [length width]
  (println "The area is:" (area length width)))

(print-area 3 4) ; => The area is: 12
```

## গভীর ডুব
অনেক আগে, কোডাররা তাদের সমস্ত লজিক একটি একক ব্লকে জড়ো করে ফেলত। এটি ছিল বিশ্রী। তারপর গঠনমূলক প্রোগ্রামিং এর আগমন ঘটে, এবং ফাংশনগুলি একটি বিষয় হয়ে উঠে। Clojure-এ, প্রতিটি ফাংশন প্রথম শ্রেণীর—আপনি তাদের অন্যান্য মানের মতই ব্যবহার করতে পারেন।

বিকল্প? কিছু লোক মাল্টি-মেথডস বা হাইয়ার-অর্ডার ফাংশনগুলি নিয়ে খেলতে পারে, কিন্তু এগুলি শুধু ফাংশন স্টু-এ মশলা মাত্র।

একটি ফাংশনের সব বিবরণ: তারা Clojure-এ অপরিবর্তনীয়, যা পার্শ্ব-প্রভাবের জটিলতা কম সম্ভাবনার করে তোলে। তারা সাধারণ লুপের পরিবর্তে পুনরাবৃত্তির উপর ভারীভাবে নির্ভর করে, যা ভাষার ফাংশনাল আদর্শের সাথে ভালভাবে মিশ্রিত।

## দেখুন এছাড়াও
- Clojure নিজস্ব গাইড: https://clojure.org/guides/learn/functions
- ফাংশনাল প্রোগ্রামিং বেসিকস: https://www.braveclojure.com/core-functions-in-depth/
- রিচ হিকির কথোপকথন: https://changelog.com/posts/rich-hickeys-greatest-hits - Clojure দর্শনের উপর দৃষ্টিপাত।

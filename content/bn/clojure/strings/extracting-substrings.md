---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:07.141184-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u0995\u09CD\u09B2\u09CB\u099C\
  \u09BE\u09B0 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A8\u09BF\u09AF\u09BC\
  \u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE \u09B8\u09B9\u099C \u0995\u09B0\u09C7\
  \ \u09A4\u09CB\u09B2\u09C7\u0964 \u0989\u09AA-\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u098F\u0995\u09CD\u09B8\u099F\u09CD\u09B0\u09BE\u0995\u09CD\u099F \u0995\
  \u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF, `subs` \u0986\u09AA\u09A8\u09BE\u09B0\
  \ \u09AF\u09BE\u0993\u09AF\u09BC\u09BE\u09B0 \u09AB\u09BE\u0982\u09B6\u09A8."
lastmod: '2024-03-17T18:47:43.608839-06:00'
model: gpt-4-0125-preview
summary: "\u0995\u09CD\u09B2\u09CB\u099C\u09BE\u09B0 \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE\
  \ \u09B8\u09B9\u099C \u0995\u09B0\u09C7 \u09A4\u09CB\u09B2\u09C7\u0964 \u0989\u09AA\
  -\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u0995\u09CD\u09B8\u099F\u09CD\
  \u09B0\u09BE\u0995\u09CD\u099F \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF\
  , `subs` \u0986\u09AA\u09A8\u09BE\u09B0 \u09AF\u09BE\u0993\u09AF\u09BC\u09BE\u09B0\
  \ \u09AB\u09BE\u0982\u09B6\u09A8."
title: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AC\u09C7\u09B0\
  \ \u0995\u09B0\u09BE"
weight: 6
---

## কিভাবে:
ক্লোজার স্ট্রিং নিয়ে কাজ করা সহজ করে তোলে। উপ-স্ট্রিং এক্সট্রাক্ট করার জন্য, `subs` আপনার যাওয়ার ফাংশন:

```clojure
(let [text "ClojureRocks"]
  (subs text 7)) ; => "Rocks"

(let [text "ClojureRocks"]
  (subs text 0 7)) ; => "Clojure"
```

এবং তাইতো—একটি শুরুর ইন্ডেক্স দিন, এবং ঐচ্ছিকভাবে একটি শেষ ইন্ডেক্স, এবং আপনি স্ট্রিংটিকে ঠিক যেমন আপনি দরকার মত কাটা পাবেন।

## গভীর ডাইভ
উপ-স্ট্রিং এক্সট্রাক্ট করা নতুন নয়—প্রোগ্রামিং-এর প্রাথমিক দিনগুলিতে থেকেই আছে। ক্লোজারে, `subs` একটি সরল ফাংশন। এটি ক্লোজারের জাভা ইন্টারপ ক্ষমতাগুলির অংশ, জাভার `substring` মেথডের উপর ভর করে। দুটি মুখ্য পয়েন্ট: নেগেটিভ ইন্ডেক্স অনুমোদিত নয়, এবং এটি জিরো-বেসড (শূন্য থেকে গণনা শুরু)। সুতরাং মনে রাখুন অন্যথায় আপনি এক বিন্দু পরিমাণ ভুল হবেন।

বিকল্প? অবশ্যই। জটিল প্যাটার্নের জন্য `re-find` এবং `re-matcher` সাথে রেগেক্স, অথবা যদি আপনি একটি ডিলিমিটারের কাছে ভাগ করতে চান তবে `split`। প্রতিটি টুলের তার নিজস্ব স্থান আছে, কিন্তু সহজতার জন্য `subs`-এর কোনও তুলনা নেই।

বাস্তবায়নের দিক থেকে, `subs` অক্ষরগুলি কপি করে না, এটি মূল স্ট্রিং এর অক্ষর অ্যারেটি শেয়ার করে। কার্যকর, কিন্তু যদি আপনার মূল স্ট্রিং প্রচুর বড় এবং আপনার শুধুমাত্র একটি ছোট্ট অংশের দরকার, আপনি অজান্তেই পুরো বড় স্ট্রিং মেমরিতে রাখতে পারেন।

## আরো দেখুন:
- অফিসিয়াল ক্লোজার স্ট্রিং API: [clojure.string](https://clojuredocs.org/clojure.string)
- জাভা `substring`: কারণ এটিই `subs`-এর পেছনের শক্তির উৎস। [জাভা substring](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#substring(int,%20int))
- ক্লোজারে রেগুলার এক্সপ্রেশনস: [re-find](https://clojuredocs.org/clojure.core/re-find)
- ক্লোজারে স্ট্রিং ভাগ করা: [split](https://clojuredocs.org/clojure.string/split)

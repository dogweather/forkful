---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:56.542219-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Clojure \u098F \u098F\u0995\u099F\
  \u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09DF\u09C7\u09B0 \u09A6\u09C8\
  \u09B0\u09CD\u0998\u09CD\u09AF \u09AA\u09C7\u09A4\u09C7, `count` \u09AB\u09BE\u0982\
  \u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C1\u09A8\
  ."
lastmod: '2024-03-17T18:47:43.611280-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u098F \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982\u09DF\u09C7\u09B0 \u09A6\u09C8\u09B0\u09CD\u0998\u09CD\u09AF \u09AA\u09C7\
  \u09A4\u09C7, `count` \u09AB\u09BE\u0982\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09C1\u09A8."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09A6\u09C8\u09B0\
  \u09CD\u0998\u09CD\u09AF \u099A\u09BF\u09B9\u09CD\u09A8\u09BF\u09A4 \u0995\u09B0\
  \u09BE"
weight: 7
---

## কিভাবে:
Clojure এ একটি স্ট্রিংয়ের দৈর্ঘ্য পেতে, `count` ফাংশন ব্যবহার করুন:

```clojure
(count "Hello, World!") ;=> 13
```

এর মানে "Hello, World!" স্ট্রিংটির 13টি অক্ষর আছে।

## গভীরে ভ্রমণ
`count` ফাংশনটি Clojure এ একটি সংগ্রহের মধ্যে থাকা আইটেমের সংখ্যা খুঁজে পেতে যাওয়ার জন্য প্রথম পছন্দ, এবং স্ট্রিং ব্যতিক্রম নয় যেহেতু এগুলি অক্ষরের একটি ক্রম হিসেবে বিবেচিত হতে পারে। ঐতিহাসিকভাবে, `count` Clojure এর আদি সংস্করণগুলি থেকেই ছিল, যা এর Lisp শিকড়ের প্রতিফলন করে, যেখানে লিস্টগুলিতে দৈর্ঘ্য অপারেশনগুলি সাধারণ।

`count` এর বিকল্প হিসাবে Java interop ব্যবহার করা যেতে পারে কারণ Clojure JVM এ চালানো হয়:

```clojure
(.length "Hello, World!") ;=> 13
```

এটি Java এর String ক্লাসের `.length` মেথডটি ডাকে। যদিও এই বিকল্প আছে, `count` ব্যবহার করা Clojure এর বেশি প্রাতিষ্ঠানিক।

উল্লেখ্য যে `count` স্ট্রিংয়ের জন্য O(1) অপারেশন, যার অর্থ হচ্ছে স্ট্রিং লেংথ যেকোনো হোক না কেন, এটি একটি স্থির সময়ের প্রয়োজন হয় কারণ স্ট্রিং লেংথ মেটাডাটা ক্যাশে সংরক্ষিত হয়।

## দেখে নিন
- `count` সম্পর্কে Clojure অফিসিয়াল ডকস: [https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/count](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/count)

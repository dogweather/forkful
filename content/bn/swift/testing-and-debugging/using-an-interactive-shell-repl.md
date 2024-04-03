---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:24:16.342669-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: REPL \u099A\u09BE\u09B2\u09C1\
  \ \u0995\u09B0\u09A4\u09C7 \u098F\u0995\u099F\u09BF \u099F\u09BE\u09B0\u09CD\u09AE\
  \u09BF\u09A8\u09BE\u09B2 \u0996\u09C1\u09B2\u09C1\u09A8 \u098F\u09AC\u0982 `swift`\
  \ \u09B0\u09BE\u09A8 \u0995\u09B0\u09C1\u09A8\u0964 \u0995\u09CB\u09A1 \u09B2\u09BF\
  \u0996\u09C1\u09A8 \u098F\u09AC\u0982 Enter \u099A\u09BE\u09AA\u09C1\u09A8 \u098F\
  \u099F\u09BF \u09B0\u09BE\u09A8 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF\
  \u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u0989\u09A6\u09BE\
  \u09B9\u09B0\u09A3."
lastmod: '2024-03-17T18:47:44.411283-06:00'
model: gpt-4-0125-preview
summary: "REPL \u099A\u09BE\u09B2\u09C1 \u0995\u09B0\u09A4\u09C7 \u098F\u0995\u099F\
  \u09BF \u099F\u09BE\u09B0\u09CD\u09AE\u09BF\u09A8\u09BE\u09B2 \u0996\u09C1\u09B2\
  \u09C1\u09A8 \u098F\u09AC\u0982 `swift` \u09B0\u09BE\u09A8 \u0995\u09B0\u09C1\u09A8\
  \u0964 \u0995\u09CB\u09A1 \u09B2\u09BF\u0996\u09C1\u09A8 \u098F\u09AC\u0982 Enter\
  \ \u099A\u09BE\u09AA\u09C1\u09A8 \u098F\u099F\u09BF \u09B0\u09BE\u09A8 \u0995\u09B0\
  \u09BE\u09B0 \u099C\u09A8\u09CD\u09AF\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\
  \u0995\u099F\u09BF \u0989\u09A6\u09BE\u09B9\u09B0\u09A3."
title: "\u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AF\u09BC\u09BE\u0995\u09CD\u099F\u09BF\
  \u09AD \u09B6\u09C7\u09B2 (REPL) \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09BE"
weight: 34
---

## কিভাবে:
REPL চালু করতে একটি টার্মিনাল খুলুন এবং `swift` রান করুন। কোড লিখুন এবং Enter চাপুন এটি রান করার জন্য। এখানে একটি উদাহরণ:

```Swift
1> let greeting = "Hello, REPL!"
greeting: String = "Hello, REPL!"
2> print(greeting)
Hello, REPL!
```

`exit` অথবা `Control-D` ব্যবহার করে বের হন।

## গভীর ডাইভ
REPL-এর মূল ষাটের দশকের Lisp ইন্টারপ্রেটারে ফিরে যায়। Swift-এর REPL LLVM, একটি শক্তিশালী কম্পাইলার ফ্রেমওয়ার্কের ওপর অবস্থিত, যা কেবল মৌলিক ইন্টারপ্রেটেশনই অফার করে না—এটি একটি পূর্ণাঙ্গ টুল যাতে অটোকমপ্লিট, ডিবাগিং, এবং আরো অনেক কিছু আছে। REPL শিখা বা প্রোটোটাইপিং-এর জন্য দারুণ, কিন্তু এটি একটি স্বতন্ত্র বিকাশের পরিবেশ নয়। কিছু মানুষ Xcode-এর Playgrounds-এ আরও গ্রাফিকাল, ফাইল-ভিত্তিক প্রক্রিয়াকে প্রাধান্য দেয়, যখন অন্যরা ঐতিহ্যগত স্ক্রিপ্ট এডিটিং এবং রানিং-এ আটকে থাকে।

আড়ালে, Swift-এর REPL কোডকে ডায়নামিক্যালি মেশিন ভাষায় কম্পাইল করে এবং তা রান করে, যা এটিকে অপেক্ষাকৃত দ্রুত করে তোলে। এটি যেকোনো কম্পাইল করা Swift মডিউল, এমনকি C লাইব্রেরিগুলিও অ্যাক্সেস করতে পারে, যা এটিকে বেশ শক্তিশালী করে তোলে। তবে মনে রাখবেন, সব কিছু REPL-এ নিখুঁতভাবে কাজ করে না; কিছু Swift বৈশিষ্ট্য, বিশেষ করে যেগুলো Complex প্রকল্প সেটআপ বা storyboard ফাইল দাবি করে, এখানে কাজ করবে না।

## আরও দেখুন
- [Swift.org - শুরু করা](https://www.swift.org/getting-started/#using-the-repl)
- Apple-এর [Xcode Playgrounds-এ পরিচীতি](https://developer.apple.com/videos/play/wwdc2014/408/)
- [LLVM Project](https://llvm.org/)

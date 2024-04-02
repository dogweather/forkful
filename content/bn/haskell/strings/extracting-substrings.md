---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:57.930870-06:00
description: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AC\u09C7\
  \u09B0 \u0995\u09B0\u09BE\u09B0 \u0985\u09B0\u09CD\u09A5 \u098F\u0995\u099F\u09BE\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A8\u09BF\
  \u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u0985\u0982\u09B6 \u0986\u09B2\u09BE\
  \u09A6\u09BE \u0995\u09B0\u09C7 \u09AC\u09C7\u09B0 \u0995\u09B0\u09BE\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\
  \u09BF \u09A1\u09C7\u099F\u09BE \u0986\u09B2\u09BE\u09A6\u09BE \u0995\u09B0\u09C7\
  \ \u09AC\u09C7\u09B0 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF, \u098F\u099F\
  \u09BF \u09AA\u09B0\u09BF\u09B7\u09CD\u0995\u09BE\u09B0 \u0995\u09B0\u09BE\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u0985\u09A5\u09AC\u09BE\u2026"
lastmod: '2024-03-17T18:47:44.070358-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AC\u09C7\
  \u09B0 \u0995\u09B0\u09BE\u09B0 \u0985\u09B0\u09CD\u09A5 \u098F\u0995\u099F\u09BE\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A8\u09BF\
  \u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u0985\u0982\u09B6 \u0986\u09B2\u09BE\
  \u09A6\u09BE \u0995\u09B0\u09C7 \u09AC\u09C7\u09B0 \u0995\u09B0\u09BE\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\
  \u09BF \u09A1\u09C7\u099F\u09BE \u0986\u09B2\u09BE\u09A6\u09BE \u0995\u09B0\u09C7\
  \ \u09AC\u09C7\u09B0 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF, \u098F\u099F\
  \u09BF \u09AA\u09B0\u09BF\u09B7\u09CD\u0995\u09BE\u09B0 \u0995\u09B0\u09BE\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u0985\u09A5\u09AC\u09BE\u2026"
title: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AC\u09C7\u09B0\
  \ \u0995\u09B0\u09BE"
weight: 6
---

## কি এবং কেন?

সাবস্ট্রিং বের করার অর্থ একটা স্ট্রিং থেকে নির্দিষ্ট অংশ আলাদা করে বের করা। প্রোগ্রামাররা এটি ডেটা আলাদা করে বের করার জন্য, এটি পরিষ্কার করার জন্য অথবা পুরোর পরিবর্তে অংশ নিয়ে কাজ করার জন্য করে থাকেন।

## কিভাবে:

হাস্কেলে, আপনি `take`, `drop`, এবং `substring` (`Data.Text` থেকে) এর মতো বিল্ট-ইন ফাংশন ব্যবহার করে স্ট্রিং কাটাকুটি করতে পারেন।

```haskell
import Data.Text (Text, pack, unpack, take, drop)

-- আমাদের উদাহরণের স্ট্রিং
let exampleStr = "Haskell makes sense!"

-- প্রথম 7টি অক্ষর নেওয়া
print $ unpack (take 7 (pack exampleStr)) -- "Haskell"

-- প্রথম 8টি অক্ষর বাদ দেওয়া
print $ unpack (drop 8 (pack exampleStr)) -- "makes sense!"

-- অবস্থান এবং দৈর্ঘ্য দ্বারা সাবস্ট্রিং এক্সট্র্যাক্ট করার কাস্টম ফাংশন
substring :: Int -> Int -> Text -> Text
substring start length = take length . drop start

-- "makes" এক্সট্র্যাক্ট করা (অবস্থান 8 থেকে শুরু করে, দৈর্ঘ্য 5)
print $ unpack (substring 8 5 (pack exampleStr)) -- "makes"
```

নমুনা আউটপুট:
```
"Haskell"
"makes sense!"
"makes"
```

## গভীরে যাওয়া

হাস্কেলে সাবস্ট্রিং বের করা অনেক কাল ধরে রয়েছে। প্রাথমিকভাবে, এটি তালিকার উপর নির্ভর করতো, কারণ স্ট্রিংগুলি অক্ষরের তালিকা। পারফরম্যান্স খুব ভালো ছিল না। `Data.Text` এর আগমন এফিসিয়েন্ট স্ট্রিং অপারেশন দিয়েছে।

বিকল্পগুলি তালিকা অপারেশন, রেগেক্স, এবং পার্সিং লাইব্রেরি। তালিকা অপস্ সহজ কিন্তু বড় স্ট্রিং-র জন্য ধীর। রেগেক্স শক্তিশালী কিন্তু সাধারণ কাজের জন্য বেশি। পার্সিং লাইব্রেরিগুলি জটিল পার্সিং এর জন্য, কিন্তু সাবস্ট্রিং নিয়েও সামাল দিতে পারে।

`Data.Text` থেকে `take` এবং `drop` ব্যবহার করে হাস্কেলে কাস্টম সাবস্ট্রিং ফাংশন বাস্তবায়ন করা সরাসরি কিন্তু তালিকা ভিত্তিক অপারেশনের চেয়ে দ্রুত স্ট্রিং হ্যান্ডলিং প্রদান করে।

## দেখুন ও

- `Data.Text` মডিউলের ডকুমেন্টেশন: https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html
- Haskell শিখুন অনায়াসে Great Good! Haskell স্ট্রিং নিয়ে সহজ ডুব: http://learnyouahaskell.com/starting-out#immutability
- বাস্তবিক ব্যবহারের কেস জন্য Real World Haskell: http://book.realworldhaskell.org/read/
- কমিউনিটির অন্তর্দৃষ্টি জন্য Haskell Wiki: https://wiki.haskell.org/How_to_work_with_strings

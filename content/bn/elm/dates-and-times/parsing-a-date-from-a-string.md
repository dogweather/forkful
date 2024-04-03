---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:06:27.995953-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Elm \u098F \u09A4\u09BE\u09B0\u09BF\
  \u0996 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u0985\u09A8\u09CD\u09AF \u0995\u09BF\u099B\u09C1 \u09AD\u09BE\u09B7\u09BE\
  \u09B0 \u09AE\u09A4\u09CB \u0985\u09AD\u09CD\u09AF\u09A8\u09CD\u09A4\u09B0\u09C0\
  \u09A3 \u09B8\u0995\u09CD\u09B7\u09AE\u09A4\u09BE \u09A8\u09C7\u0987, \u09AA\u09CD\
  \u09B0\u09A7\u09BE\u09A8\u09A4 \u099C\u09BE\u09AD\u09BE\u09B8\u09CD\u0995\u09CD\u09B0\
  \u09BF\u09AA\u09CD\u099F \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AA \u0985\u09A5\
  \u09AC\u09BE \u099C\u099F\u09BF\u09B2 \u0985\u09AA\u09BE\u09B0\u09C7\u09B6\u09A8\
  \ \u098F\u09B0 \u099C\u09A8\u09CD\u09AF \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\
  \u09B0\u09BF \u0989\u09AA\u09B0\u2026"
lastmod: '2024-03-17T18:47:43.958183-06:00'
model: gpt-4-0125-preview
summary: "Elm \u098F \u09A4\u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u09B0\u09CD\u09B8\
  \ \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u0985\u09A8\u09CD\u09AF \u0995\
  \u09BF\u099B\u09C1 \u09AD\u09BE\u09B7\u09BE\u09B0 \u09AE\u09A4\u09CB \u0985\u09AD\
  \u09CD\u09AF\u09A8\u09CD\u09A4\u09B0\u09C0\u09A3 \u09B8\u0995\u09CD\u09B7\u09AE\u09A4\
  \u09BE \u09A8\u09C7\u0987, \u09AA\u09CD\u09B0\u09A7\u09BE\u09A8\u09A4 \u099C\u09BE\
  \u09AD\u09BE\u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F \u0987\u09A8\u09CD\
  \u099F\u09BE\u09B0\u09AA \u0985\u09A5\u09AC\u09BE \u099C\u099F\u09BF\u09B2 \u0985\
  \u09AA\u09BE\u09B0\u09C7\u09B6\u09A8 \u098F\u09B0 \u099C\u09A8\u09CD\u09AF \u09B2\
  \u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u0989\u09AA\u09B0 \u09A8\u09BF\
  \u09B0\u09CD\u09AD\u09B0 \u0995\u09B0\u09C7\u0964 \u09A4\u09AC\u09C7, \u09AE\u09CC\
  \u09B2\u09BF\u0995 \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982\u09DF\u09C7\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u0986\u09AA\u09A8\u09BF `elm/time` \u09AA\u09CD\u09AF\
  \u09BE\u0995\u09C7\u099C \u098F\u09AC\u0982 \u0986\u09B0\u09CB \u099C\u099F\u09BF\
  \u09B2 \u09AA\u09CD\u09B0\u09DF\u09CB\u099C\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u09A4\u09C3\u09A4\u09C0\u09DF-\u09AA\u0995\u09CD\u09B7\u09C7\u09B0 `justinmimbs/date`\
  \ \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u099F\u09BF \u09AA\u09CD\
  \u09B0\u09B6\u0982\u09B8\u09BF\u09A4\u0964\n\n#."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A4\
  \u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 30
---

## কিভাবে:
Elm এ তারিখ পার্স করার জন্য অন্য কিছু ভাষার মতো অভ্যন্তরীণ সক্ষমতা নেই, প্রধানত জাভাস্ক্রিপ্ট ইন্টারপ অথবা জটিল অপারেশন এর জন্য লাইব্রেরি উপর নির্ভর করে। তবে, মৌলিক পার্সিংয়ের জন্য আপনি `elm/time` প্যাকেজ এবং আরো জটিল প্রয়োজনের জন্য তৃতীয়-পক্ষের `justinmimbs/date` লাইব্রেরিটি প্রশংসিত।

### `elm/time` ব্যবহারে পার্সিং:
`elm/time` `Time` মডিউল প্রদান করে, যা আপনাকে মানব-পঠনযোগ্য তারিখের পরিবর্তে সময়মাপক (timestamps) এর সাথে কাজ করতে দেয়। এটি সরাসরি স্ট্রিং থেকে তারিখ পার্স করে না, কিন্তু আপনি একটি ISO 8601 স্ট্রিংকে একটি POSIX সময়মাপকে রূপান্তর করতে পারেন, যা আপনি তারপর কাজে লাগাতে পারেন।

```elm
import Time exposing (Posix)

-- ধরা যাক আপনার কাছে একটি ISO 8601 তারিখ স্ট্রিং আছে
isoDateStr : String
isoDateStr = "2023-01-01T00:00:00Z"

-- এটিকে একটি POSIX সময়মাপকে রূপান্তর করুন (এই ফাংশনটি `Result` ফেরত দেয়)
parsedDate : Result String Posix
parsedDate = Time.fromIsoString8601 isoDateStr

-- নমুনা আউটপুট: Ok <posix time value>
```

### `justinmimbs/date` ব্যবহারে পার্সিং:
আরও জটিল পার্সিং এর জন্য, যেমন নন-ISO ফর্ম্যাট সামলানো, `justinmimbs/date` লাইব্রেরি দুর্দান্ত একটি পছন্দ। এখানে কিভাবে আপনি এটি ব্যবহার করে একটি কাস্টম তারিখ স্ট্রিং পার্স করতে পারেন:

1. নিশ্চিত করুন আপনি লাইব্রেরিটি ইনস্টল করেছেন:

```shell
elm install justinmimbs/date
```

2. কাস্টম তারিখ ফর্ম্যাট পার্স করতে `Date.fromString` ফাংশন ব্যবহার করুন:

```elm
import Date
import Result exposing (Result(..))

-- ধরুন, আপনার কাছে একটি কাস্টম তারিখ স্ট্রিং ফর্ম্যাট `dd-MM-yyyy` আছে
customDateStr : String
customDateStr = "01-01-2023"

-- কাস্টম ফর্ম্যাট পার্স করার ফাংশন
parseDate : String -> Result String Date.Date
parseDate = Date.fromString "dd-MM-yyyy"

-- নমুনা ব্যবহার
parsedCustomDate : Result String Date.Date
parsedCustomDate = parseDate customDateStr

-- নমুনা আউটপুট: Ok (Date.fromCalendarDate 2023 Jan 1)
```

এই উদাহরণগুলিতে, `Result` টাইপ হয় সফল পার্সিং যা একটি তারিখ পরিণত (`Ok`) অথবা একটি ত্রুটি (`Err`) encapsulate করে, আপনার Elm অ্যাপ্লিকেশনগুলিতে শক্তিশালী ত্রুটি হ্যান্ডেলিং সক্ষম করে।

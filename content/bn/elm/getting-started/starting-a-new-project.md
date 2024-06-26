---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:20:23.453990-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Elm-\u098F, \u0986\u09AA\u09A8\
  \u09BE\u09B0 \u09AA\u09CD\u09B0\u0995\u09B2\u09CD\u09AA\u09C7\u09B0 \u09A1\u09BF\
  \u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF\u09A4\u09C7 \u09A8\u09C7\u09AD\u09BF\u0997\
  \u09C7\u099F \u0995\u09B0\u09C7 `elm init` \u0995\u09AE\u09BE\u09A8\u09CD\u09A1\u09C7\
  \u09B0 \u09B8\u09BE\u09A5\u09C7 \u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09C1\u09A8\
  \u0964 \u099F\u09BE\u09B0\u09CD\u09AE\u09BF\u09A8\u09BE\u09B2 \u099A\u09BE\u09B2\
  \u09C1 \u0995\u09B0\u09C7."
lastmod: '2024-03-17T18:47:43.949253-06:00'
model: gpt-4-0125-preview
summary: "Elm-\u098F, \u0986\u09AA\u09A8\u09BE\u09B0 \u09AA\u09CD\u09B0\u0995\u09B2\
  \u09CD\u09AA\u09C7\u09B0 \u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF\u09A4\
  \u09C7 \u09A8\u09C7\u09AD\u09BF\u0997\u09C7\u099F \u0995\u09B0\u09C7 `elm init`\
  \ \u0995\u09AE\u09BE\u09A8\u09CD\u09A1\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09B6\
  \u09C1\u09B0\u09C1 \u0995\u09B0\u09C1\u09A8\u0964 \u099F\u09BE\u09B0\u09CD\u09AE\
  \u09BF\u09A8\u09BE\u09B2 \u099A\u09BE\u09B2\u09C1 \u0995\u09B0\u09C7."
title: "\u09A8\u09A4\u09C1\u09A8 \u09AA\u09CD\u09B0\u0995\u09B2\u09CD\u09AA \u09B6\
  \u09C1\u09B0\u09C1 \u0995\u09B0\u09BE"
weight: 1
---

## কিভাবে:
Elm-এ, আপনার প্রকল্পের ডিরেক্টরিতে নেভিগেট করে `elm init` কমান্ডের সাথে শুরু করুন। টার্মিনাল চালু করে:

```shell
mkdir my-elm-project
cd my-elm-project
elm init
```

এই কমান্ডটি একটি `elm.json` ফাইল এবং `src` ডিরেক্টরি তৈরি করে। এখানে Elm-এ একটি সরল "Hello, World!" উদাহরণ দেওয়া হল:

```Elm
module Main exposing (..)

import Html exposing (text)

main =
    text "Hello, World!"
```

`elm reactor` দিয়ে যখন আপনি এটি চালাবেন এবং `http://localhost:8000` ভিজিট করবেন, তখন আপনার ব্রাউজারে "Hello, World!" দেখাবে।

## গভীর অন্বেষণ
Elm ২০১২ সালে এসেছিল ফ্রন্ট-এন্ড ডেভেলপমেন্টকে আরও আনন্দদায়ক করার লক্ষ্যে। এটি শুধু রানটাইম ত্রুটি এড়ানো নয়; Elm ডেভেলপারদের সুখ এবং সারল্যের ওপর গভীর মনোনিবেশ নিয়ে আসে। র‌্যাক জাভাস্ক্রিপ্ট লেখা অথবা React এর মতো ফ্রেমওয়ার্কগুলি ব্যবহার করার মতো অনেক বিকল্পের তুলনায়, Elm নিজস্ব একটি ভাষা। শক্তিশালী টাইপিং এবং নিখুঁত ফাংশনগুলির সাথে, এটি পূর্বানুমানযোগ্যতা এবং রক্ষণাবেক্ষণ নিয়ে আসে।

যখন আপনি একটি নতুন Elm প্রকল্প শুরু করেন, আপনি Elm আর্কিটেকচারকেও গ্রহণ করছেন, এটি আপনার ওয়েব অ্যাপসগুলিকে গঠন করার একটি প্যাটার্ন যা সারল্য এবং স্কেলেবিলিটির ওপর জোর দেয়। এটি আপনার পুরো অ্যাপ্লিকেশন স্টেট এবং তার কীভাবে আপডেট হয় সেটি একত্রিত করে। `create-elm-app` এর মতো অন্যান্য টুলগুলি আরও জটিল সেটআপস তৈরি করতে পারে, তবে `elm init` দিয়ে শুরু করা যতটা লিন হতে পারে ততটাই হয়।

## আরও দেখুন
- Elm অফিসিয়াল গাইড: https://guide.elm-lang.org/
- Elm আর্কিটেকচার টিউটোরিয়াল: https://guide.elm-lang.org/architecture/
- Elm টুলিং: `create-elm-app`: https://github.com/halfzebra/create-elm-app
- Elm প্যাকেজ ক্যাটালগ: https://package.elm-lang.org/

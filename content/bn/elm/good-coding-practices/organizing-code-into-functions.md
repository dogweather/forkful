---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:55:37.233018-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u098F\u0996\u09BE\u09A8\u09C7\
  \ \u098F\u0995\u099F\u09BF \u09B8\u09BE\u09AE\u09BE\u09A8\u09CD\u09AF \u09AB\u09BE\
  \u0982\u09B6\u09A8 \u09B8\u09B9 Elm \u0995\u09CB\u09A1\u09C7\u09B0 \u098F\u0995\u099F\
  \u09BF \u0996\u09A3\u09CD\u09A1 \u09A6\u09C7\u0993\u09DF\u09BE \u09B9\u09B2\u09CB\
  , \u09AF\u09C7\u099F\u09BF \u098F\u0995\u099C\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0\u0995\u09BE\u09B0\u09C0\u0995\u09C7 \u09B8\u09CD\u09AC\u09BE\u0997\u09A4\
  \ \u099C\u09BE\u09A8\u09BE\u09AF\u09BC."
lastmod: '2024-03-17T18:47:43.954199-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09BE\u09AE\
  \u09BE\u09A8\u09CD\u09AF \u09AB\u09BE\u0982\u09B6\u09A8 \u09B8\u09B9 Elm \u0995\u09CB\
  \u09A1\u09C7\u09B0 \u098F\u0995\u099F\u09BF \u0996\u09A3\u09CD\u09A1 \u09A6\u09C7\
  \u0993\u09DF\u09BE \u09B9\u09B2\u09CB, \u09AF\u09C7\u099F\u09BF \u098F\u0995\u099C\
  \u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\u09C0\u0995\u09C7\
  \ \u09B8\u09CD\u09AC\u09BE\u0997\u09A4 \u099C\u09BE\u09A8\u09BE\u09AF\u09BC."
title: "\u0995\u09CB\u09A1\u0995\u09C7 \u09AB\u09BE\u0982\u09B6\u09A8\u0997\u09C1\u09B2\
  \u09BF\u09A4\u09C7 \u09B8\u09BE\u099C\u09BE\u09A8\u09CB"
weight: 18
---

## কিভাবে:
এখানে একটি সামান্য ফাংশন সহ Elm কোডের একটি খণ্ড দেওয়া হলো, যেটি একজন ব্যবহারকারীকে স্বাগত জানায়:

```Elm
module Main exposing (..)

import Html exposing (text)

greetUser : String -> String
greetUser userName =
    "Hello, " ++ userName ++ "!"

main =
    text (greetUser "Casey")
```

এটি চালান, এবং আপনি আউটপুট পাবেন: "Hello, Casey!"

এখন, ধরুন আপনি আরও ব্যক্তিগতকরণ যোগ করতে চান। আরও ফাংশনালিটি এক্সট্র্যাক্ট করুন!

```Elm
module Main exposing (..)

import Html exposing (text)

greetUser : String -> String -> String
greetUser greeting userName =
    greeting ++ ", " ++ userName ++ "!"

personalGreeting : String -> String
personalGreeting userName =
    greetUser "Howdy" userName

main =
    text (personalGreeting "Casey")
```

এখন, যখন আপনি এটি চালাবেন: "Howdy, Casey!" জাদু? না, শুধু ফাংশন তার কাজ করছে।

## গভীর ডাইভ
পুরানো দিনে, কোড প্রায়শই নির্দেশাবলীর একটি দীর্ঘ সিকোয়েন্স হতো (স্প্যাগেটি কোড ভাবুন)। এটি রক্ষণাবেক্ষণ করা একটি দুঃস্বপ্ন ছিল। তারপর গঠনমূলক প্রোগ্রামিং এসেছিল, এবং এর সাথে, ফাংশন। Elm, এর ফাংশনাল প্রোগ্রামিং পূর্বসূরিদের মতো, সংগঠনের জন্য গভীরভাবে ফাংশনে নির্ভর করে।

আপনি ফাংশনের মধ্যে ফাংশন নেস্ট করতে পারেন, ক্লোজার তৈরি করতে পারেন, অথবা সরলতার জন্য তাদের পিওর রাখতে পারেন। Elm পরেরটি উৎসাহিত করে: পিওর ফাংশন যাদের ভালোভাবে সংজ্ঞায়িত ইনপুট এবং আউটপুট থাকে, যা ডিবাগিং এবং পরীক্ষা করা সহজ করে।

Elm ফাংশন হাইয়ার-অর্ডার হতে পারে, অর্থাৎ তারা অন্যান্য ফাংশন গ্রহণ করতে পারে অথবা ফেরত দিতে পারে। এটি সমন্বয়যোগ্যতার একটি বিশ্ব খুলে দেয়। তবে, অন্য কিছু ভাষায়ের মতো না, Elm-এ ফাংশন ওভারলোডিং নেই; প্রতিটি ফাংশনের একটি অনন্য নাম থাকতে হবে।

Elm একটি শক্তিশালী স্ট্যাটিক টাইপিং সিস্টেম চাপিয়ে দেয় যা শুধুমাত্র টাইপগুলি চেক করে না, তা তাদের অনুমান করে, বয়লারপ্লেট কোড কমায়।

অন্যান্য ভাষার প্রক্রিয়াজাত বা অবজেক্ট-ওরিয়েন্টেড কোড সংগঠনের তুলনায়, Elm-এর পদ্ধতি সরলতা এবং পূর্বাভাসযোগ্যতা জোর দেয়। Elm-এ অবজেক্ট বা ক্লাস নেই। আপনি কোড আয়োজন করতে ক্লাস এবং ইন্সট্যান্সের পরিবর্তে ফাংশন এবং মডিউল ব্যবহার করেন।

## আরও দেখুন
আরও গভীরে গিয়ে জানতে, এই সম্পদগুলি দেখুন:
- ফাংশনগুলি সম্পর্কে Elm-এর অফিসিয়াল গাইড: https://guide.elm-lang.org/core_language.html
- জটিল ফাংশনের উদাহরণের জন্য Elm প্যাকেজ ডকুমেন্টেশন: https://package.elm-lang.org/
- Elm-এর টাইপ সিস্টেম সম্পর্কে জানুন, যা ফাংশন সংগঠনের সাথে সুন্দরভাবে মেলে: https://elm-lang.org/docs/types

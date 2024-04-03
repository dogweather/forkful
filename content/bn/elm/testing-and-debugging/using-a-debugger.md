---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:22:15.853175-06:00
description: "\u0990\u09A4\u09BF\u09B9\u09CD\u09AF\u0997\u09A4 \u0985\u09B0\u09CD\u09A5\
  \u09C7 Elm-\u098F \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u09A1\u09BF\u09AC\
  \u09BE\u0997\u09BE\u09B0 \u09A8\u09C7\u0987, \u09AF\u09C7\u09AE\u09A8\u099F\u09BE\
  \ \u09AC\u09B2\u09BE \u09AF\u09BE\u09DF, JavaScript-\u098F \u09AC\u09CD\u09B0\u09BE\
  \u0989\u099C\u09BE\u09B0 \u09A1\u09C7\u09AD \u099F\u09C1\u09B2\u09B8 \u09B8\u09BE\
  \u09A5\u09C7 \u0986\u099B\u09C7\u0964 \u09A4\u09AC\u09C7, Elm \u09B8\u09AE\u09CD\
  \u09AA\u09CD\u09B0\u09A6\u09BE\u09AF\u09BC \u098F\u0987 \u09AB\u09BE\u0981\u0995\
  \u099F\u09BF \u09AA\u09C2\u09B0\u09A3\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u099F\
  \u09C1\u09B2\u09B8 \u09A4\u09C8\u09B0\u09BF\u2026"
lastmod: '2024-03-17T18:47:43.953139-06:00'
model: gpt-4-0125-preview
summary: "\u0990\u09A4\u09BF\u09B9\u09CD\u09AF\u0997\u09A4 \u0985\u09B0\u09CD\u09A5\
  \u09C7 Elm-\u098F \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u09A1\u09BF\u09AC\
  \u09BE\u0997\u09BE\u09B0 \u09A8\u09C7\u0987, \u09AF\u09C7\u09AE\u09A8\u099F\u09BE\
  \ \u09AC\u09B2\u09BE \u09AF\u09BE\u09DF, JavaScript-\u098F \u09AC\u09CD\u09B0\u09BE\
  \u0989\u099C\u09BE\u09B0 \u09A1\u09C7\u09AD \u099F\u09C1\u09B2\u09B8 \u09B8\u09BE\
  \u09A5\u09C7 \u0986\u099B\u09C7\u0964 \u09A4\u09AC\u09C7, Elm \u09B8\u09AE\u09CD\
  \u09AA\u09CD\u09B0\u09A6\u09BE\u09AF\u09BC \u098F\u0987 \u09AB\u09BE\u0981\u0995\
  \u099F\u09BF \u09AA\u09C2\u09B0\u09A3\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u099F\
  \u09C1\u09B2\u09B8 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09C7\u099B\u09C7\u0964\
  \ \u0986\u09AA\u09A8\u09BE\u09B0 Elm \u0985\u09CD\u09AF\u09BE\u09AA \u09A1\u09BF\
  \u09AC\u09BE\u0997 \u0995\u09B0\u09A4\u09C7 `elm-debug-transformer` \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BE\u09B0 \u09A8\u09BF\u09B0\u09CD\
  \u09A6\u09C7\u09B6\u09BF\u0995\u09BE \u09A8\u09BF\u09AE\u09CD\u09A8\u09B0\u09C2\u09AA\
  :\n\n```Elm\n-- elm-debug-transformer \u0987\u09A8\u09B8\u09CD\u099F\u09B2 \u0995\
  \u09B0\u09C1\u09A8 (\u09A8\u09CB\u09A1 \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C\
  )\n\n1."
title: "\u09A1\u09BF\u09AC\u09BE\u0997\u09BE\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09BE"
weight: 35
---

## কিভাবে:
ঐতিহ্যগত অর্থে Elm-এ বিল্ট-ইন ডিবাগার নেই, যেমনটা বলা যায়, JavaScript-এ ব্রাউজার ডেভ টুলস সাথে আছে। তবে, Elm সম্প্রদায় এই ফাঁকটি পূরণের জন্য টুলস তৈরি করেছে। আপনার Elm অ্যাপ ডিবাগ করতে `elm-debug-transformer` ব্যবহার করার নির্দেশিকা নিম্নরূপ:

```Elm
-- elm-debug-transformer ইনস্টল করুন (নোড প্যাকেজ)

1. npm install -g elm-debug-transformer

-- আপনার অ্যাপ শুরু করতে elm-debug-transformer ব্যবহার করুন

2. elm-debug-transformer --port=8000 yourMainElmFile.elm 
```

`elm-debug-transformer` চালু হয়ে গেলে, এটি লগিংয়ের জন্য একটি WebSocket সংযোগ তৈরি করে। আপনার ব্রাউজারের কনসোলে আপনি ডিবাগ তথ্য দেখতে পাবেন যেখানে আপনি আপনার অ্যাপ্লিকেশনের নির্দিষ্ট পয়েন্টে আপনার প্রোগ্রামের ডেটা স্ট্রাকচারগুলি পরিদর্শন করতে পারেন।

Elm 0.19 এবং পরবর্তীতে, `Debug` মডিউলের ফাংশন যেমন `Debug.log` এবং `Debug.todo` আপনার মানগুলি ট্র্যাক করতে এবং আপনার কোডের অসম্পূর্ণ অংশগুলি সচেতনভাবে চিহ্নিত করতে সাহায্য করতে পারে। Debug.log ব্যবহার করার উপায় এখানে দেওয়া হল:

```Elm
import Debug

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( Debug.log "Incrementing" { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( Debug.log "Decrementing" { model | count = model.count - 1 }, Cmd.none )
```

আপনি আপনার ব্রাউজারের কনসোলে "Incrementing" অথবা "Decrementing" বার্তাগুলি দেখতে পাবেন যা সাথে `model`-এর নতুন অবস্থা দেখাবে।

## গভীর ডুব
Elm-এর লেখক, ইভান চাজপ্লিকি, এমন এক ভাষা তৈরি করতে চেয়েছিলেন যেখানে সাধারণ ত্রুটিগুলি অসম্ভব অথবা সহজে ধরা পড়বে৤ এই দর্শনই কেন Elm-এর মূলে ঐতিহ্যগত ডিবাগিং ফাংশন অন্তর্ভুক্ত নেই। Elm-এর স্ট্যাটিক বিশ্লেষণ এবং টাইপ ইনফারেন্স রানটাইম ত্রুটিগুলি কমাতে বিশাল অবদান রাখে, যা জটিল রানটাইম ডিবাগিংয়ের প্রয়োজন হ্রাস করে। ইতিহাসগত বিকল্পগুলি অন্তর্ভুক্ত করে এখন অপ্রচলিত `elm-reactor` ব্যবহার করা যা সময়-ভ্রমণ ডিবাগিং অফার করে - আপনার অ্যাপে ক্রিয়াকলাপগুলি পুনরায় উল্টানো এবং পুনরায় চালানোর একটি উপায়।

আজ, `elm-debug-transformer` এবং Elm-এর `Debug` মডিউলের ব্যবহার ফাঁকটি পূরণ করে। যদিও `Debug` মডিউলটি বিকাশ পর্যায়ে ব্যবহারের জন্য উদ্দিষ্ট এবং উৎপাদন নির্মাণের আগে মুছে ফেলা উচিত, এটি রাষ্ট্র পরিবর্তনগুলি নির্দিষ্ট করা এবং লগিং করার জন্য একটি অমূল্য সরঞ্জাম।

মনে রাখবেন যে Elm-এর স্থাপত্য এবং Elm রানটাইমের স্টেট আপডেট পরিচালনা করার কারণে, ব্রেকপয়েন্ট বা ধাপে ধাপে নির্বাহের মতো ঐতিহ্যগত JavaScript ডিবাগিং কৌশল সরাসরি Elm-এ প্রযোজ্য নয়। Elm আপনাকে আপনার প্রোগ্রামকে এমনভাবে গঠন করার উপদেশ দেয় যাতে ডেটা প্রবাহ স্পষ্ট হয় এবং কঠোর টাইপ এবং অপরিবর্তনীয়তা গ্যারান্টি অনুসরণ করে, যা ডিবাগিং প্রয়োজনের ক্ষেত্রগুলি ন্যূনতম করে।

## আরও দেখুন
- রানটাইম ব্যাত্যয়গুলি হ্যান্ডলিং সম্পর্কে Elm-এর অফিসিয়াল গাইড: https://guide.elm-lang.org/error_handling/
- `elm-debug-transformer` GitHub রিপোজিটরি: https://github.com/kraklin/elm-debug-transformer
- ডিবাগিং কৌশল নিয়ে আলোচনা করা Elm ডিসকোর্স থ্রেড: https://discourse.elm-lang.org/c/show-and-tell/debugging
- Elm-এর `Debug` মডিউল ডকুমেন্টেশন: https://package.elm-lang.org/packages/elm/core/latest/Debug

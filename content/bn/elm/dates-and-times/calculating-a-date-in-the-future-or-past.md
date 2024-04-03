---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:34.839061-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Elm \u098F\u09B0 `Time` \u09AE\
  \u09A1\u09BF\u0989\u09B2 \u098F\u09AC\u0982 `justinmimbs/time-extra` \u09AA\u09CD\
  \u09AF\u09BE\u0995\u09C7\u099C \u0986\u09AE\u09BE\u09A6\u09C7\u09B0\u0995\u09C7\
  \ \u09A4\u09BE\u09B0\u09BF\u0996 \u09A8\u09BF\u09DF\u09C7 \u09B8\u09B9\u099C\u09C7\
  \ \u0996\u09C7\u09B2\u09BE\u09B0 \u09B8\u09C1\u09AF\u09CB\u0997 \u0995\u09B0\u09C7\
  \ \u09A6\u09C7\u09DF\u0964."
lastmod: '2024-03-17T18:47:43.962270-06:00'
model: gpt-4-0125-preview
summary: "Elm \u098F\u09B0 `Time` \u09AE\u09A1\u09BF\u0989\u09B2 \u098F\u09AC\u0982\
  \ `justinmimbs/time-extra` \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C \u0986\u09AE\
  \u09BE\u09A6\u09C7\u09B0\u0995\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996 \u09A8\u09BF\
  \u09DF\u09C7 \u09B8\u09B9\u099C\u09C7 \u0996\u09C7\u09B2\u09BE\u09B0 \u09B8\u09C1\
  \u09AF\u09CB\u0997 \u0995\u09B0\u09C7 \u09A6\u09C7\u09DF\u0964."
title: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u09AC\u09BE \u0985\u09A4\u09C0\
  \u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u0997\u09A3\u09A8\u09BE \u0995\
  \u09B0\u09BE"
weight: 26
---

## কিভাবে:
Elm এর `Time` মডিউল এবং `justinmimbs/time-extra` প্যাকেজ আমাদেরকে তারিখ নিয়ে সহজে খেলার সুযোগ করে দেয়।

```Elm
import Time exposing (Posix)
import Time.Extra as TimeExtra

--calculateDate : Int -> Posix -> Posix
-- @deltaDays: যোগ করার দিনের সংখ্যা (নেগেটিভ মান বিয়োগ করবে)
-- @fromDate: প্রারম্ভিক তারিখ পজিক্স ফরম্যাটে

calculateDate deltaDays fromDate =
    TimeExtra.add TimeExtra.days deltaDays fromDate

-- ব্যবহার
-- মনে রাখবেন, Elm সময় গণনা করে মিলিসেকেন্ডে Unix epoch থেকে।

sampleDate = Time.millisToPosix 1580515200000  -- ফেব্রুয়ারী 1, 2020 00:00:00 UTC
futureDate = calculateDate 10 sampleDate       -- 10 দিন যোগ করে
pastDate = calculateDate -15 sampleDate        -- 15 দিন বিয়োগ করে

-- নমুনা আউটপুট:
-- futureDate -> 1581552000000  -- ফেব্রুয়ারী 12, 2020 00:00:00 UTC
-- pastDate -> 1580006400000    -- জানুয়ারী 17, 2020 00:00:00 UTC
```

## গভীর ডুব
অতীতে, প্রোগ্রামিংয়ে তারিখ সামলানো এক প্রকারের ব্যাথা ছিল। বিভিন্ন সিস্টেম, ফরম্যাট, এবং টাইম জোনস সবাইকে মাথাব্যাথা দিত। Elm এর `Time` মডিউলটি, যা Unix Time সিস্টেমের উপর নির্ভরশীল (১৯৭০ সাল থেকে মিলিসেকেন্ডে সময়), এটি স্ট্যান্ডার্ড করে তোলে। `justinmimbs/time-extra` প্যাকেজটি তারিখের উপর অপারেশন যেমন দিন যোগ বা বিয়োগ আরও সহজ করে দেয়।

বিকল্প? অন্যান্য ভাষায় নিজস্ব লাইব্রেরি আছে, যেমন Python এর `datetime` অথবা JavaScript এর `Date`। কিন্তু Elm এর পদ্ধতি শক্তিশালী টাইপিং এবং বিশুদ্ধতা প্রদান করে, যা বাগ কমায়।

শুধু দিন যোগ করা নয়, আপনি মাস, বছর, অথবা এমনকি ঘণ্টা এবং মিনিট নিয়েও কাজ করতে পারেন। Elm এবং `time-extra` মতো প্যাকেজগুলির ফাংশনগুলি অপরিবর্তনীয়তা এবং বিশুদ্ধ ফাংশনগুলিতে মনোনিবেশ করে—এর মানে কোন পার্শ্বপ্রতিক্রিয়া নেই। যখন আপনি একটি নতুন তারিখ হিসাব করেন, মূল তারিখটি অপরিবর্তিত থাকে।

## আরও দেখুন
- Elm `Time` মডিউল: https://package.elm-lang.org/packages/elm/time/latest/
- `justinmimbs/time-extra` প্যাকেজ: https://package.elm-lang.org/packages/justinmimbs/time-extra/latest/
- Elm গাইড টাইমে: https://guide.elm-lang.org/effects/time.html

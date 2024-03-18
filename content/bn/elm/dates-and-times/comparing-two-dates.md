---
title:                "দুটি তারিখ তুলনা করা"
date:                  2024-03-17T17:45:46.859295-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
দুটি তারিখের তুলনা মানে এর মধ্যে কোনটি আগে বা এদের মধ্যে কত সময় ব্যবধান রয়েছে তা নির্ধারণ করা। প্রোগ্রামারগণ মেয়াদপূর্তি, সূচি, অথবা সময়-ভিত্তিক বৈশিষ্ট্যাদি সামলানোর জন্য এটি করে থাকে।

## কিভাবে:

এলম দুটি তারিখের তুলনা সরল করে তোলে। ধরা যাক, আপনার কাছে দুটি তারিখ রয়েছে। এখানে আপনি কিভাবে চেক করবেন কোনটি আগে:

```Elm
import Time exposing (Posix)
import Date

compareDates : Posix -> Posix -> Order
compareDates date1 date2 =
    if date1 < date2 then
        LT  -- date1 টি date2 এর চেয়ে আগে
    else if date1 > date2 then
        GT  -- date1 টি date2 এর চেয়ে পরে
    else
        EQ  -- তারিখগুলো একই

-- নমুনা ব্যবহার:
let
    date1 = Date.fromPosix <| Time.millisToPosix 1650931200000 -- POSIX সময়ে আপনার প্রথম তারিখ যোগ করুন
    date2 = Date.fromPosix <| Time.millisToPosix 1651017600000 -- এবং আপনার দ্বিতীয় তারিখ POSIX সময়ে
in
compareDates date1 date2
-- আউটপুট হয়ত LT, GT, বা EQ হবে
```

আপনি মিলিসেকেন্ডে পার্থক্য নির্ণেয় করতে পারেনঃ

```Elm
timeDifference : Posix -> Posix -> Time.Duration
timeDifference date1 date2 =
    Time.millisToPosix date1 - Time.millisToPosix date2

-- নমুনা ব্যবহার:
let
    date1 = Date.fromPosix <| Time.millisToPosix 1650931200000
    date2 = Date.fromPosix <| Time.millisToPosix 1651017600000
in
timeDifference date1 date2
-- আউটপুট: মিলিসেকেন্ডে সময়ের দৈর্ঘ্য
```

## গভীর ডুব
এলম তারিখগুলিকে `Posix` হিসাবে সংরক্ষণ করে, যা ইউনিক্স এপোচ (১ জানুয়ারী ১৯৭০, UTC) থেকে মিলিসেকেন্ডের গণনা প্রতিনিধিত্ব করে। এটি একটি সাধারণ প্রক্রিয়া যা ইউনিক্স সময়ের সাথে তার উৎস শেয়ার করে, এবং এটি তারিখ ম্যানিপুলেশন এবং সংরক্ষণকে সহজ করে দেয়। 

যদিও এলমের কোর লাইব্রেরি মৌলিক তারিখ নিয়ন্ত্রণ প্রদান করে, কিছু বিকল্প যেমন `justinmimbs/date` আরও জটিল অপারেশনের জন্য অস্তিত্বে রয়েছে।

তারিখের তুলনা প্রয়োগ করার সময়, স্মরণে রাখবেন সময় অঞ্চল জিনিসটিকে জটিল করে তুলতে পারে। এলমের `Time` মডিউল UTC ধরে নেয়, যার অর্থ আপনি daylight saving এর ঘাড় থেকে মুক্ত, তবে আপনার অ্যাপ্লিকেশনে স্থানীয় সময় অঞ্চলের সামঞ্জস্য করা প্রয়োজন হতে পারে।

## আরও দেখুন
- এলম টাইম মডিউল: https://package.elm-lang.org/packages/elm/time/latest/
- জাস্টিন মিম্বসের এলমে তারিখের প্যাকেজ: https://package.elm-lang.org/packages/justinmimbs/date/latest/
- ইউনিক্স সময়: https://en.wikipedia.org/wiki/Unix_time

---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:54.167850-06:00
description: "\u09A4\u09CD\u09B0\u09C1\u099F\u09BF \u09B8\u09BE\u09AE\u09B2\u09BE\u09A8\
  \u09CB \u09AE\u09BE\u09A8\u09C7 \u098F\u09AE\u09A8 \u0995\u09CB\u09A1 \u09B2\u09C7\
  \u0996\u09BE \u09AF\u09BE \u09AD\u09C1\u09B2 \u09AC\u09BE \u0997\u09B2\u09A4\u09BF\
  \u09B0 \u09AA\u09CD\u09B0\u09A4\u09CD\u09AF\u09BE\u09B6\u09BE \u0995\u09B0\u09A4\
  \u09C7 \u09AA\u09BE\u09B0\u09C7 \u098F\u09AC\u0982 \u09A4\u09BE \u09B8\u09BE\u09AE\
  \u09B2\u09BE\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\
  \u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u09A8 \u0995\u09CD\u09B0\u09CD\u09AF\u09BE\
  \u09B6 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B0\u09CB\u09A7, \u09A1\u09BE\u099F\u09BE\
  \ \u09B8\u09A4\u09A4\u09BE \u09B0\u0995\u09CD\u09B7\u09BE\u2026"
lastmod: '2024-03-17T18:47:43.956183-06:00'
model: gpt-4-0125-preview
summary: "\u09A4\u09CD\u09B0\u09C1\u099F\u09BF \u09B8\u09BE\u09AE\u09B2\u09BE\u09A8\
  \u09CB \u09AE\u09BE\u09A8\u09C7 \u098F\u09AE\u09A8 \u0995\u09CB\u09A1 \u09B2\u09C7\
  \u0996\u09BE \u09AF\u09BE \u09AD\u09C1\u09B2 \u09AC\u09BE \u0997\u09B2\u09A4\u09BF\
  \u09B0 \u09AA\u09CD\u09B0\u09A4\u09CD\u09AF\u09BE\u09B6\u09BE \u0995\u09B0\u09A4\
  \u09C7 \u09AA\u09BE\u09B0\u09C7 \u098F\u09AC\u0982 \u09A4\u09BE \u09B8\u09BE\u09AE\
  \u09B2\u09BE\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\
  \u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u09A8 \u0995\u09CD\u09B0\u09CD\u09AF\u09BE\
  \u09B6 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B0\u09CB\u09A7, \u09A1\u09BE\u099F\u09BE\
  \ \u09B8\u09A4\u09A4\u09BE \u09B0\u0995\u09CD\u09B7\u09BE\u2026"
title: "\u09A4\u09CD\u09B0\u09C1\u099F\u09BF \u09AA\u09B0\u09BF\u099A\u09BE\u09B2\u09A8\
  \u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

ত্রুটি সামলানো মানে এমন কোড লেখা যা ভুল বা গলতির প্রত্যাশা করতে পারে এবং তা সামলাতে পারে। প্রোগ্রামাররা এটি করে থাকেন ক্র্যাশ প্রতিরোধ, ডাটা সততা রক্ষা এবং ব্যবহারকারীদের জন্য সুন্দর অপসারণ প্রদান করার জন্য।

## কিভাবে:

এলমের মূল দর্শন হল নো রানটাইম এক্সেপশন। তাই, এলম তার টাইপ সিস্টেমকে `Maybe` এবং `Result` এর মতো টাইপগুলোর মাধ্যমে ত্রুটি সামলানোর জন্য ব্যবহার করে।

`Maybe` সিনারিওর জন্য:

```Elm
safeDivide : Float -> Float -> Maybe Float
safeDivide numerator denominator =
    if denominator == 0 then
        Nothing
    else
        Just (numerator / denominator)
        
-- যখন আপনি এটি চালান:

safeDivide 10 2
--> Just 5

safeDivide 10 0
--> Nothing
```

`Result` সিনারিওর জন্য:

```Elm
type Error = DivisionByZero

safeDivide : Float -> Float -> Result Error Float
safeDivide numerator denominator =
    if denominator == 0 then
        Err DivisionByZero
    else
        Ok (numerator / denominator)

-- এবং এটি ব্যবহার করে:

safeDivide 10 2
--> Ok 5

safeDivide 10 0
--> Err DivisionByZero
```

## গভীর ডাইভ
এলমের টাইপ সিস্টেম কঠোর, যা ত্রুটি সম্পর্কে শীঘ্রই জানাতে সাহায্য করে। ঐতিহাসিকভাবে, বেশিরভাগ ভাষাগুলি এক্সেপশনগুলি এবং রানটাইম চেকগুলিতে নির্ভর করেছিল, কিন্তু এলম কম্পাইল-টাইম গ্যারান্টিতে সিদ্ধান্ত নিয়েছে। `Result` এর মতো বিকল্পগুলি বিস্তারিত ত্রুটি তথ্য প্রদান করে, যেখানে `Maybe` হ্যাঁ-না সিনারিওর জন্য সহজতর। এলমের ত্রুটি সামলানো ডেভেলপারদেরকে পূর্বে সব পথ বিবেচনা করতে উৎসাহিত করে, ভুলে যাওয়া ত্রুটি কেসগুলির জন্য পতনের ফাঁদ এড়ানোর জন্য।

## দেখুন ও:
- এলমের অফিশিয়াল গাইড বিভাগ ত্রুটি সামলানো প্রসঙ্গে: [ত্রুটি সামলানো – একটি পরিচিতি](https://guide.elm-lang.org/error_handling/)
- এলম `Maybe` ডকুমেন্টেশন: [এলম – Maybe](https://package.elm-lang.org/packages/elm/core/latest/Maybe)
- এলম `Result` ডকুমেন্টেশন: [এলম – Result](https://package.elm-lang.org/packages/elm/core/latest/Result)

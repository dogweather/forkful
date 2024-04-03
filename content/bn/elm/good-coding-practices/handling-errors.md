---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:54.167850-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u098F\u09B2\u09AE\u09C7\u09B0\
  \ \u09AE\u09C2\u09B2 \u09A6\u09B0\u09CD\u09B6\u09A8 \u09B9\u09B2 \u09A8\u09CB \u09B0\
  \u09BE\u09A8\u099F\u09BE\u0987\u09AE \u098F\u0995\u09CD\u09B8\u09C7\u09AA\u09B6\u09A8\
  \u0964 \u09A4\u09BE\u0987, \u098F\u09B2\u09AE \u09A4\u09BE\u09B0 \u099F\u09BE\u0987\
  \u09AA \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u0995\u09C7 `Maybe` \u098F\u09AC\
  \u0982 `Result` \u098F\u09B0 \u09AE\u09A4\u09CB \u099F\u09BE\u0987\u09AA\u0997\u09C1\
  \u09B2\u09CB\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09A4\u09CD\u09B0\
  \u09C1\u099F\u09BF \u09B8\u09BE\u09AE\u09B2\u09BE\u09A8\u09CB\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u2026"
lastmod: '2024-03-17T18:47:43.956183-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u09B2\u09AE\u09C7\u09B0 \u09AE\u09C2\u09B2 \u09A6\u09B0\u09CD\u09B6\
  \u09A8 \u09B9\u09B2 \u09A8\u09CB \u09B0\u09BE\u09A8\u099F\u09BE\u0987\u09AE \u098F\
  \u0995\u09CD\u09B8\u09C7\u09AA\u09B6\u09A8\u0964 \u09A4\u09BE\u0987, \u098F\u09B2\
  \u09AE \u09A4\u09BE\u09B0 \u099F\u09BE\u0987\u09AA \u09B8\u09BF\u09B8\u09CD\u099F\
  \u09C7\u09AE\u0995\u09C7 `Maybe` \u098F\u09AC\u0982 `Result` \u098F\u09B0 \u09AE\
  \u09A4\u09CB \u099F\u09BE\u0987\u09AA\u0997\u09C1\u09B2\u09CB\u09B0 \u09AE\u09BE\
  \u09A7\u09CD\u09AF\u09AE\u09C7 \u09A4\u09CD\u09B0\u09C1\u099F\u09BF \u09B8\u09BE\
  \u09AE\u09B2\u09BE\u09A8\u09CB\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\u0964\n\n`Maybe` \u09B8\u09BF\u09A8\u09BE\
  \u09B0\u09BF\u0993\u09B0 \u099C\u09A8\u09CD\u09AF."
title: "\u09A4\u09CD\u09B0\u09C1\u099F\u09BF \u09AA\u09B0\u09BF\u099A\u09BE\u09B2\u09A8\
  \u09BE"
weight: 16
---

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

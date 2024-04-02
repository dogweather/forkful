---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:28.404618-06:00
description: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\
  \u09C7 \u09B2\u09CB\u09AF\u09BC\u09BE\u09B0\u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\
  \u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7\
  \ \u09B8\u09AE\u09B8\u09CD\u09A4 \u09AC\u09B0\u09CD\u09A3\u09AE\u09BE\u09B2\u09BE\
  \u09B0 \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09A4\
  \u09BE\u09A6\u09C7\u09B0 \u09B2\u09CB\u09AF\u09BC\u09BE\u09B0-\u0995\u09C7\u09B8\
  \ \u09AB\u09B0\u09CD\u09AE\u09C7 \u09AA\u09B0\u09BF\u09A3\u09A4 \u0995\u09B0\u09BE\
  \u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u09AA\u09CD\u09B0\u09BE\u09AF\u09BC\u09B6\u0987 \u098F\u099F\u09BF \u0995\u09C7\
  \u09B8-\u0987\u09A8\u09B8\u09C7\u09A8\u09B8\u09BF\u099F\u09BF\u09AD \u09A4\u09C1\
  \u09B2\u09A8\u09BE\u2026"
lastmod: '2024-03-17T18:47:43.935193-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\
  \u09C7 \u09B2\u09CB\u09AF\u09BC\u09BE\u09B0\u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\
  \u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7\
  \ \u09B8\u09AE\u09B8\u09CD\u09A4 \u09AC\u09B0\u09CD\u09A3\u09AE\u09BE\u09B2\u09BE\
  \u09B0 \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09A4\
  \u09BE\u09A6\u09C7\u09B0 \u09B2\u09CB\u09AF\u09BC\u09BE\u09B0-\u0995\u09C7\u09B8\
  \ \u09AB\u09B0\u09CD\u09AE\u09C7 \u09AA\u09B0\u09BF\u09A3\u09A4 \u0995\u09B0\u09BE\
  \u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u09AA\u09CD\u09B0\u09BE\u09AF\u09BC\u09B6\u0987 \u098F\u099F\u09BF \u0995\u09C7\
  \u09B8-\u0987\u09A8\u09B8\u09C7\u09A8\u09B8\u09BF\u099F\u09BF\u09AD \u09A4\u09C1\
  \u09B2\u09A8\u09BE\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7 \u09B2\u09CB\u09AF\u09BC\
  \u09BE\u09B0 \u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\
  \u09B0 \u0995\u09B0\u09BE"
weight: 4
---

## কি এবং কেন?

একটি স্ট্রিংকে লোয়ারকেসে রূপান্তর করা মানে সমস্ত বর্ণমালার অক্ষরগুলিকে তাদের লোয়ার-কেস ফর্মে পরিণত করা। প্রোগ্রামাররা প্রায়শই এটি কেস-ইনসেনসিটিভ তুলনা এবং স্টোরেজ এবং প্রক্রিয়াজনক জন্য পাঠ্য ডেটার সাধারণীকরণ করার জন্য করে থাকেন।

## কিভাবে:

এল্ম টেক্সট রূপান্তরের জন্য `String.toLower` ফাংশন ব্যবহার করে:

```elm
import String

lowercaseString : String -> String
lowercaseString text =
    String.toLower text

-- ব্যবহার
result : String
result =
    lowercaseString "HeLLo, WoRLD!"

-- আউটপুট: "hello, world!"
```

## গভীর ডুব

এল্মের `String.toLower` এল্মের কোর `String` লাইব্রেরি থেকে আসে, আন্তর্জাতিকীকরণ মাথায় রেখে। ঐতিহাসিকভাবে, কেস রূপান্তর বেসিক ASCII থেকে আন্তর্জাতিক টেক্সট হ্যান্ডলিং-এর প্রয়োজনে পূর্ণ Unicode সমর্থনে উন্নীত হয়েছে।

কিছু ভাষায়, যেমন জাভাস্ক্রিপ্টে, `toLowerCase()` এবং `toLocaleLowerCase()` এর মতো বিকল্প আছে, যেখানে পরেরটি লোকেল-নির্দিষ্ট নিয়মাবলী বিবেচনা করে। এল্মে, বেশিরভাগ ক্ষেত্রের জন্য `String.toLower` পর্যাপ্ত হবে, যদি না লোকেল-সেনসিটিভ অপারেশন নিয়ে কাজ করতে হয়, যা কাস্টম বাস্তবায়ন প্রয়োজন করতে পারে।

মনে রাখা দরকার যে, কেস রূপান্তর সবসময় এক-একের সমানুপাতিক নয়; কিছু অক্ষরের কোনো লোয়ারকেস সমতুল্য নাও থাকতে পারে, এবং অন্যান্যগুলো আকার পরিবর্তন করতে পারে (উদাহরণস্বরূপ, জার্মানে "ß" রূপান্তর করলে)।

## আরও দেখুন

- এল্ম স্ট্রিং ডকুমেন্টেশন: [https://package.elm-lang.org/packages/elm/core/latest/String#toLower](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- ইউনিকোড কেস ফোল্ডিং: [https://www.w3.org/International/wiki/Case_folding](https://www.w3.org/International/wiki/Case_folding)
- ভাষা-নির্দিষ্ট কেস রূপান্তর সমস্যা: [https://stackoverflow.com/questions/234591/upper-vs-lower-case](https://stackoverflow.com/questions/234591/upper-vs-lower-case)

---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:09.735032-06:00
description: "\u098F\u0995\u099F\u09BF \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\
  \u099F \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8\u09C7\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u09AE\u09BF\u09B2\u09C7 \u09AF\u09BE\u0993\u09AF\u09BC\u09BE\
  \ \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09AE\u09C1\u099B\u09C7\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2\u09CB\
  \ \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09A5\u09C7\u0995\u09C7 \u09AC\u09BF\u09B6\
  \u09C7\u09B7 \u09B8\u09C7\u099F \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\
  \u09BF \u0985\u09AA\u09B8\u09BE\u09B0\u09A3 \u0995\u09B0\u09BE, \u09A8\u09BF\u09AF\
  \u09BC\u09AE (\u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8) \u0985\u09A8\
  \u09C1\u09AF\u09BE\u09AF\u09BC\u09C0\u0964\u2026"
lastmod: '2024-03-17T18:47:43.932127-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\
  \u099F \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8\u09C7\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u09AE\u09BF\u09B2\u09C7 \u09AF\u09BE\u0993\u09AF\u09BC\u09BE\
  \ \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09AE\u09C1\u099B\u09C7\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2\u09CB\
  \ \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09A5\u09C7\u0995\u09C7 \u09AC\u09BF\u09B6\
  \u09C7\u09B7 \u09B8\u09C7\u099F \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\
  \u09BF \u0985\u09AA\u09B8\u09BE\u09B0\u09A3 \u0995\u09B0\u09BE, \u09A8\u09BF\u09AF\
  \u09BC\u09AE (\u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8) \u0985\u09A8\
  \u09C1\u09AF\u09BE\u09AF\u09BC\u09C0\u0964\u2026"
title: "\u098F\u0995\u099F\u09BF \u09A8\u09AE\u09C1\u09A8\u09BE \u09AE\u09C7\u09B2\
  \u09C7 \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09AE\u09C1\u099B\
  \u09C7 \u09AB\u09C7\u09B2\u09BE"
weight: 5
---

## কি এবং কেন?
একটি নির্দিষ্ট প্যাটার্নের সাথে মিলে যাওয়া অক্ষরগুলি মুছে দেওয়া মানে হলো টেক্সট থেকে বিশেষ সেট অক্ষরগুলি অপসারণ করা, নিয়ম (প্যাটার্ন) অনুযায়ী। প্রোগ্রামাররা টেক্সট পরিষ্কার, ডাটা প্রসেসিং, অথবা পার্সিং করার আগে ইনপুট সহজ করার জন্য এটি করে থাকেন।

## কিভাবে:
Elm সরাসরি regex সাপোর্ট করে না, তবে আপনি অক্ষর মুছে দেওয়া অনুকরণ করতে পারেন। এখানে একটি উদাহরণ আছে `String.filter` ব্যবহার করে একটি স্ট্রিং থেকে সংখ্যা অক্ষরগুলি অপসারণ করার।

```Elm
import Browser
import Html উপস্থাপন (text)

removeDigits : String -> String
removeDigits = String.filter (\char -> not (char >= '0' && char <= '9'))

main =
  text (removeDigits "Elm 0.19.1 is super 123 cool!")

-- আউটপুট: "Elm . is super  cool!"
```

## গভীরে ডুব:
Elm তার মূল ভাষায় regex অন্তর্ভুক্ত করে না, অন্যান্য অনেক ভাষার থেকে ভিন্ন। এই ডিজাইন চয়েস সহজতা এবং নিরাপত্তা জন্য Elm এর লক্ষ্যের সাথে সামঞ্জস্যপূর্ণ। Regex ভুল বোঝাবুঝি এবং ডিবাগ করা কঠিন হতে পারে, কিন্তু Elm অনেক সাধারণ ব্যবহারের ক্ষেত্রে সহজ স্ট্রিং অপারেশনগুলি উৎসাহিত করে।

Regex সত্যিই প্রয়োজন হলে, বাস্তবায়ন JavaScript ইন্টারঅপের মাধ্যমে পোর্টস নি reliance়ে হয়। তবে, Elm ভাষার মধ্যেই সমাধান খুঁজে বের করার প্রতি উৎসাহিত করে। `String` মডিউল যেমন `filter`, `replace`, এবং `split` এর মত ফাংশনগুলি প্রদান করে যা regex এর জটিলতা ছাড়া অনেক প্যাটার্ন-ভিত্।ি পাঠ্য ম্যানিপুলেশনে আবরণ করে।

## দেখুন
- [Elm স্ট্রিং ডকুমেন্টেশন](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Practical Elm for a Busy Developer](https://korban.net/elm/book/) - পুস্তক যেটি টেক্সট ম্যানিপুলেশন ইউটিলিটিজ অন্তর্ভুক্ত করে।

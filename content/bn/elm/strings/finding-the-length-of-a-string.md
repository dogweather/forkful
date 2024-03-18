---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:07.703914-06:00
description: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  -\u098F\u09B0 \u09A6\u09C8\u09B0\u09CD\u0998\u09CD\u09AF \u0996\u09C1\u0981\u099C\
  \u09C7 \u09AA\u09BE\u0993\u09AF\u09BC\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u09B0\
  \ \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u0997\u09A3\u09A8\u09BE\
  \ \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09BE\u09B0\u09B0\u09BE \u0987\u09A8\u09AA\u09C1\u099F \u09AF\u09BE\u099A\u09BE\
  \u0987 \u0995\u09B0\u09BE, \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AA\u09B0\u09BF\
  \u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\u09BE \u0985\u09A5\u09AC\u09BE \u0995\
  \u09C7\u09AC\u09B2 \u09A1\u09C7\u099F\u09BE \u0985\u09A8\u09C1\u09AE\u09BE\u09A8\
  \ \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u099F\u09BF\u2026"
lastmod: '2024-03-17T18:47:43.939190-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982-\u098F\
  \u09B0 \u09A6\u09C8\u09B0\u09CD\u0998\u09CD\u09AF \u0996\u09C1\u0981\u099C\u09C7\
  \ \u09AA\u09BE\u0993\u09AF\u09BC\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u09B0 \u0985\
  \u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u0997\u09A3\u09A8\u09BE \u0995\
  \u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \u09B0\u09BE \u0987\u09A8\u09AA\u09C1\u099F \u09AF\u09BE\u099A\u09BE\u0987 \u0995\
  \u09B0\u09BE, \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AA\u09B0\u09BF\u09AC\u09B0\
  \u09CD\u09A4\u09A8 \u0995\u09B0\u09BE \u0985\u09A5\u09AC\u09BE \u0995\u09C7\u09AC\
  \u09B2 \u09A1\u09C7\u099F\u09BE \u0985\u09A8\u09C1\u09AE\u09BE\u09A8 \u0995\u09B0\
  \u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u099F\u09BF\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09A6\u09C8\u09B0\
  \u09CD\u0998\u09CD\u09AF \u099A\u09BF\u09B9\u09CD\u09A8\u09BF\u09A4 \u0995\u09B0\
  \u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
একটি স্ট্রিং-এর দৈর্ঘ্য খুঁজে পাওয়া মানে এর অক্ষরগুলি গণনা করা। প্রোগ্রামাররা ইনপুট যাচাই করা, টেক্সট পরিবর্তন করা অথবা কেবল ডেটা অনুমান করার জন্য এটি করে থাকেন।

## কিভাবে:
Elm-এ, আপনি `String.length` ব্যবহার করে একটি স্ট্রিং-এ কতগুলি অক্ষর রয়েছে তা জানতে পারেন। দেখুন:

```elm
import Html exposing (text)

main =
  text (String.fromInt (String.length "Hello, Elm!"))
  -- আউটপুট: "11"
```

## গভীর ডুব
ঐতিহাসিকভাবে, স্ট্রিং দৈর্ঘ্য ফাংশনগুলি ডেটা অ্যাক্সেসের নিম্ন-স্তরের ভাষায় মেমোরি পরিচালনা এবং টেক্সট নিয়ন্ত্রণে অত্যন্ত গুরুত্বপূর্ণ ছিল। Elm, উচ্চ্ও স্তরের হওয়ায়, এই বিবরণগুলি লুকিয়ে রাখে, `String.length` এর সাথে নির্মিত কার্যকারিতা অফার করে।

দুইটি বিষয় লক্ষনীয়:
১. Elm স্ট্রিংগুলি UTF-16 এনকোডেড। `String.length` UTF-16 কোড ইউনিটের সংখ্যা ফেরত দেয়, যা জটিল অক্ষরের সাথে স্ট্রিংগুলিতে আসল ইউনিকোড গ্রাফিমসের (ব্যবহারকারী উপলব্ধি করা অক্ষর) সংখ্যা থেকে ভিন্ন হতে পারে।
২. Elm-এ `String.length`-এর বিকল্প নির্মিত কার্যকারিতা নেই। যদি আপনার গ্রাফিমসের সংখ্যা প্রয়োজন হয়, আপনার হয়তো একটি কাস্টম ফাংশন প্রয়োজন হতে পারে যা আপনাকে ইউনিকোডের জটিলতাগুলির জন্য হিসেব করবে।

আভ্যন্তরীণভাবে, `String.length` স্ট্রিং ডেটা কাঠামোর উপর ইটারেট করে, উপাদানগুলি গণনা করে। একটি শুদ্ধ ফাংশন হিসাবে, এর আউটপুট কেবলমাত্র ইনপুটের উপর নির্ভরশীল, Elm-এর ফাংশনাল প্রোগ্রামিং নীতিকে বজায় রাখে।

## আরও দেখুন
- Elm-এর আনুষ্ঠানিক `String` ডকুমেন্টেশন: [https://package.elm-lang.org/packages/elm/core/latest/String#length](https://package.elm-lang.org/packages/elm/core/latest/String#length)
- UTF-16: [https://en.wikipedia.org/wiki/UTF-16](https://en.wikipedia.org/wiki/UTF-16)

---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:44.544421-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Elm-\u098F, \u0986\u09AA\u09A8\
  \u09BF `Date` \u09AE\u09A1\u09BF\u0989\u09B2 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996\u0997\u09C1\u09B2\u09BF\
  \ \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09C7\u09A8, \u098F\
  \u09AC\u0982 `elm/time` \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C \u09A4\u09BE\u09B0\
  \u09BF\u0996\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982-\u098F \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE\
  \u09B0 \u099C\u09A8\u09CD\u09AF \u09AB\u09BE\u0982\u09B6\u09A8 \u09B8\u09B0\u09AC\
  \u09B0\u09BE\u09B9 \u0995\u09B0\u09C7\u0964\u2026"
lastmod: '2024-03-17T18:47:43.960323-06:00'
model: gpt-4-0125-preview
summary: "Elm-\u098F, \u0986\u09AA\u09A8\u09BF `Date` \u09AE\u09A1\u09BF\u0989\u09B2\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09A4\u09BE\u09B0\
  \u09BF\u0996\u0997\u09C1\u09B2\u09BF \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\
  \u099C \u0995\u09B0\u09C7\u09A8, \u098F\u09AC\u0982 `elm/time` \u09AA\u09CD\u09AF\
  \u09BE\u0995\u09C7\u099C \u09A4\u09BE\u09B0\u09BF\u0996\u0997\u09C1\u09B2\u09BF\u0995\
  \u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982-\u098F \u09B0\u09C2\u09AA\u09BE\
  \u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09AB\
  \u09BE\u0982\u09B6\u09A8 \u09B8\u09B0\u09AC\u09B0\u09BE\u09B9 \u0995\u09B0\u09C7\
  \u0964 \u099A\u09B2\u09C1\u09A8 \u0995\u09BF\u099B\u09C1 Elm \u0995\u09CB\u09A1\u09C7\
  \u09B0 \u09B8\u09BE\u09A5\u09C7 \u0986\u09AE\u09B0\u09BE \u09A1\u09C1\u09AC \u09A6\
  \u09BF\u0987."
title: "\u09A4\u09BE\u09B0\u09BF\u0996\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u098F \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE"
weight: 28
---

## কিভাবে:
Elm-এ, আপনি `Date` মডিউল ব্যবহার করে তারিখগুলি নিয়ে কাজ করেন, এবং `elm/time` প্যাকেজ তারিখগুলিকে স্ট্রিং-এ রূপান্তর করার জন্য ফাংশন সরবরাহ করে। চলুন কিছু Elm কোডের সাথে আমরা ডুব দিই:

```Elm
import Time exposing (Posix)
import Date

-- ধরে নেই আমাদের কাছে একটি Posix টাইমস্ট্যাম্প আছে
posixTime : Posix
posixTime = Time.millisToPosix 1672569600000

-- Posix টাই একটি Date-এ রূপান্তর করা 
date : Date.Date
date = Date.fromPosix posixTime

-- তারিখটিকে একটি স্ট্রিং হিসাবে ফরম্যাট করা
dateToString : String
dateToString = Date.toIsoString date

-- আউটপুট
dateToString --> "2023-01-01T00:00:00.000Z"
```

লাইন `Date.toIsoString date` টি আসলে ভারী কাজ করে - আপনার `Date.Date` মানটিকে ISO 8601 ফরমেটের স্ট্রিং এ পরিণত করে।

## গভীর ডুব
ঐতিহাসিকভাবে, Elm তারিখ এবং সময় নিয়ে তার অভিগমন ভাষার সাথে বিকশিত হয়েছে, আরও নির্ভুলতা এবং সামঞ্জস্যের জন্য লক্ষ্য রেখে। `elm/time` প্যাকেজ ব্যবহার করে, Elm সময় নিপুণতার প্রক্রিয়া সরল করে।

তারিখগুলি রূপান্তর করার বিকল্পগুলি অন্তর্ভুক্ত করে কাস্টম ফরম্যাটারের ব্যবহার যদি আপনি তারিখগুলি দেখানোর জন্য একটি নির্দিষ্ট উপায় চান। `Date` মডিউল নিজে বিস্তৃত ফরম্যাটিং অপশন সরবরাহ করে না, অর্থাৎ যদি আপনার ISO 8601 এর বাইরে কিছু প্রয়োজন হয়, তাহলে আপনি `justinmimbs/date` এর মতো কমিউনিটি প্যাকেজের দিকে মোড় নিতেন আরও ফরম্যাটিং নমনীয়তার জন্�্য।

বাস্তবায়ণের দিক থেকে, যখন আপনি Elm-এ একটি তারিখকে স্ট্রিং-এ রূপান্তর করেন, আপনি অবচেতন ভাবে টাইম জোনস নিয়ে কাজ করেন। Elm আদর্শগত ভাবে তারিখগুলিকে UTC-এ উপস্থাপন করে, যার মানে হল রূপান্তরের সময় কোন অনাকাঙ্ক্ষিত সময়ের পরিবর্তন নেই, যদি না আপনি অতিরিক্ত যুক্তির সাহায্যে স্পষ্টভাবে সময় অঞ্চল পরিচালনা করেন। এই ডিজাইন পছন্দটি ভিন্ন সময় অঞ্চলের সার্ভার এবং ক্লায়েন্ট নিয়ে কাজ করার সময় বাগ এবং অসামঞ্জস্য হ্রাস করার লক্ষ্যে।

## আরো দেখুন
- Elm `Time` প্যাকেজ: [Elm Time](https://package.elm-lang.org/packages/elm/time/latest/)
- কমিউনিটি ডেট ফরম্যাটিং: [justinmimbs/date](https://package.elm-lang.org/packages/justinmimbs/date/latest/)
- Elm ডেট গাইড: [Elm Guide - Time](https://guide.elm-lang.org/effects/time.html)

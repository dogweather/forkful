---
changelog:
- 2024-01-31, dogweather, reviewed
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:23:59.921801-06:00
description: "Read-Eval-Print Loop (REPL) \u09B9\u09B2 \u098F\u0995\u099F\u09BF \u09B8\
  \u09BE\u09A7\u09BE\u09B0\u09A3, \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09C7\u0995\
  \u09CD\u099F\u09BF\u09AD \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\
  \u0982 \u09AA\u09B0\u09BF\u09AC\u09C7\u09B6, \u09AF\u09BE \u098F\u0995\u0995 \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\u09C0\u09B0 \u0987\u09A8\u09AA\
  \u09C1\u099F \u0997\u09CD\u09B0\u09B9\u09A3 \u0995\u09B0\u09C7, \u09A4\u09BE \u09AE\
  \u09C2\u09B2\u09CD\u09AF\u09BE\u09AF\u09BC\u09A8 \u0995\u09B0\u09C7 \u098F\u09AC\
  \u0982 \u09AB\u09B2\u09BE\u09AB\u09B2 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \u0995\u09BE\u09B0\u09C0\u09B0\u2026"
lastmod: '2024-03-17T18:47:43.950248-06:00'
model: gpt-4-0125-preview
summary: "Read-Eval-Print Loop (REPL) \u09B9\u09B2 \u098F\u0995\u099F\u09BF \u09B8\
  \u09BE\u09A7\u09BE\u09B0\u09A3, \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09C7\u0995\
  \u09CD\u099F\u09BF\u09AD \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\
  \u0982 \u09AA\u09B0\u09BF\u09AC\u09C7\u09B6, \u09AF\u09BE \u098F\u0995\u0995 \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\u09C0\u09B0 \u0987\u09A8\u09AA\
  \u09C1\u099F \u0997\u09CD\u09B0\u09B9\u09A3 \u0995\u09B0\u09C7, \u09A4\u09BE \u09AE\
  \u09C2\u09B2\u09CD\u09AF\u09BE\u09AF\u09BC\u09A8 \u0995\u09B0\u09C7 \u098F\u09AC\
  \u0982 \u09AB\u09B2\u09BE\u09AB\u09B2 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \u0995\u09BE\u09B0\u09C0\u09B0 \u0995\u09BE\u099B\u09C7 \u09AB\u09BF\u09B0\u09BF\
  \u09AF\u09BC\u09C7 \u09A6\u09C7\u09AF\u09BC\u0964 Elm \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE REPL \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09C7 \u09A6\u09CD\u09B0\u09C1\u09A4 \u09AA\u09B0\u09C0\
  \u0995\u09CD\u09B7\u09BE, \u09A1\u09BF\u09AC\u09BE\u0997\u09BF\u0982 \u0985\u09A5\
  \u09AC\u09BE \u09AD\u09BE\u09B7\u09BE\u099F\u09BF \u09B6\u09C7\u0996\u09BE\u09B0\
  \ \u099C\u09A8\u09CD\u09AF\u0964."
title: "\u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AF\u09BC\u09BE\u0995\u09CD\u099F\u09BF\
  \u09AD \u09B6\u09C7\u09B2 (REPL) \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09BE"
weight: 34
---

## কিভাবে:
Elm এর সাথে একটি সংহত REPL আসে। আপনার কমান্ড লাইন থেকে `elm repl` ব্যবহার করে Elm সেশন শুরু করতে:

```Elm
> import List exposing (..)
> map (\x -> x * 2) [1, 2, 3, 4]
[2,4,6,8] : List number
```

এই সেশনে, List ফাংশন আমদানি করার পর, আমরা একটি লিস্টের সংখ্যাগুলি দ্বিগুণ করেছি এবং তৎক্ষণাৎ ফলাফল পেয়েছি।

`elm repl --help` একটু তথ্য দেখায়:

```
$ elm repl --help
`repl` কমান্ড একটি ইন্টারেক্টিভ প্রোগ্রামিং সেশন খোলে:

    elm repl

এটা ব্যবহার করে কিভাবে শেখার জন্য <https://guide.elm-lang.org> দিয়ে কাজ করা শুরু করুন। এটা
একটি সম্পূর্ণ অধ্যায় আছে যেখানে REPL সবকিছুর জন্য ব্যবহৃত হয়েছে, তাই এটা সম্ভবত
শুরু করার জন্য দ্রুততম পথ।

আপনি নিম্নলিখিত ফ্ল্যাগগুলি দিয়ে এই কমান্ড কাস্টমাইজ করতে পারেন:

    --interpreter=<interpreter>
        এল্টারনেট JS ইন্টারপ্রিটারের পাথ, যেমন node অথবা nodejs.

    --no-colors
        REPL এ রংগুলি বন্ধ করুন। যদি আপনি মানগুলি পড়তে সমস্যা হচ্ছে তাহলে এটি সাহায্য করতে পারে।
        কিছু টার্মিনাল কাস্টম রং স্কিম ব্যবহার করে যা স্ট্যান্ডার্ড ANSI রং থেকে ব্যাপকভাবে
        প্রত্যাহার করে, তাই আরেকটি পথ হতে পারে আরও স্ট্যান্ডার্ড রং স্কিম বেছে নেওয়া।
```

## গভীর ডুব
Elm's REPL Python বা JavaScript এর মত কিছু অন্যান্য ভাষার REPL গুলির তুলনায় সীমাবদ্ধ মনে হতে পারে, যেহেতু Elm একটি কম্পাইলড ভাষা যা ওয়েব অ্যাপস তৈরিতে ফোকাস করে। ঐতিহাসিকভাবে, Elm মূল্যবান অ্যাপ্লিকেশনের উপর ফোকাস করে স্ক্রিপ্টিং বা শেল ইন্টারাকশনের চেয়ে।

Elm's REPL এর বিকল্পগুলি অন্তর্ভুক্ত `elm-live` এবং Ellie এর মতো অনলাইন এডিটরগুলি যেখানে আপনি ব্রাউজারে রিয়েল-টাইমে কোডে পরিবর্তন দেখতে পারেন।

বাস্তবায়নের ব্যাপারে, Elm REPL পটভূমিতে Elm কোডের স্নিপেটগুলি JavaScript এ কম্পাইল করে, যা আপনাকে Elm কে ইন্টারেকটিভভাবে চালানোর সুযোগ দেয়। এটি ইন্টারপ্রিটেড ভাষার REPL গুলি থেকে পৃথক, যার জন্য এই কম্পাইলেশন ধাপ প্রয়োজন নেই। Elm REPL কোর ভাষাকে হালকা এবং ফোকাস রাখার জন্য সংকুচিত করাও আছে।

## আরও দেখুন
- Elm এর অফিসিয়াল গাইড: https://guide.elm-lang.org/
- Ellie, একটি অনলাইন Elm খেলার মাঠ: https://ellie-app.com/new
- `elm-live`, Elm এর জন্য একটি নমনীয় ডেভ সার্ভার: https://www.elm-live.com/

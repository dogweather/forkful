---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:32.817424-06:00
description: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AF\u09C1\u0995\u09CD\u09A4\
  \ \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09A6\u09C1\u0987 \u09AC\u09BE \u09A4\
  \u09A4\u09CB\u09A7\u09BF\u0995 \u099F\u09C7\u0995\u09CD\u09B8\u099F \u099F\u09C1\
  \u0995\u09B0\u09CB\u0995\u09C7 \u098F\u0995\u09A4\u09CD\u09B0 \u0995\u09B0\u09BE\
  \u0964 \u098F\u099F\u09BE \u09A0\u09BF\u0995 \u09A1\u09BE\u0995\u09CD\u099F \u099F\
  \u09C7\u09AA \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BE\u09B0\
  \ \u09AE\u09A4\u09CB\u0987 \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3 \u098F\u09AC\u0982\
  \ \u099C\u09B0\u09C1\u09B0\u09C0, \u09AF\u09BE \u0986\u09AA\u09A8\u09BE\u0995\u09C7\
  \ \u09AC\u09BE\u09B0\u09CD\u09A4\u09BE \u09AA\u09CD\u09B0\u09A6\u09B0\u09CD\u09B6\
  \u09A8, \u099F\u09C7\u09AE\u09CD\u09AA\u09B2\u09C7\u099F\u2026"
lastmod: '2024-03-17T18:47:43.940285-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AF\u09C1\u0995\u09CD\u09A4\
  \ \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09A6\u09C1\u0987 \u09AC\u09BE \u09A4\
  \u09A4\u09CB\u09A7\u09BF\u0995 \u099F\u09C7\u0995\u09CD\u09B8\u099F \u099F\u09C1\
  \u0995\u09B0\u09CB\u0995\u09C7 \u098F\u0995\u09A4\u09CD\u09B0 \u0995\u09B0\u09BE\
  \u0964 \u098F\u099F\u09BE \u09A0\u09BF\u0995 \u09A1\u09BE\u0995\u09CD\u099F \u099F\
  \u09C7\u09AA \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BE\u09B0\
  \ \u09AE\u09A4\u09CB\u0987 \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3 \u098F\u09AC\u0982\
  \ \u099C\u09B0\u09C1\u09B0\u09C0, \u09AF\u09BE \u0986\u09AA\u09A8\u09BE\u0995\u09C7\
  \ \u09AC\u09BE\u09B0\u09CD\u09A4\u09BE \u09AA\u09CD\u09B0\u09A6\u09B0\u09CD\u09B6\
  \u09A8, \u099F\u09C7\u09AE\u09CD\u09AA\u09B2\u09C7\u099F \u09A4\u09C8\u09B0\u09BF\
  , \u098F\u09AC\u0982 \u0986\u09B0\u09CB \u0985\u09A8\u09C7\u0995 \u0995\u09BF\u099B\
  \u09C1 \u099C\u09A8\u09CD\u09AF \u09A8\u09A4\u09C1\u09A8 \u09B8\u09CD\u099F\u09CD\
  \u09B0\u09BF\u0982 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE\u09B0 \u09B8\u09C1\
  \u09AF\u09CB\u0997 \u09A6\u09C7\u09DF\u0964."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u099C\u09CB\u09A1\u09BC\u09BE\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE"
weight: 3
---

## কিভাবে:
এলম এর একটি চমৎকার অপারেটর `(++)` আছে এই উদ্দেশ্যের জন্য:

```Elm
greeting : String
greeting =
    "Hello, " ++ "world!"

-- "Hello, world!"
```

কিন্তু মাঝে মাঝে, আপনার অনেকগুলো টুকরো থাকতে পারে। ভয় পাবেন না, `++` চেইনেবল হয়:

```Elm
fullName : String
fullName =
    "Elm" ++ " " ++ "Lang"

-- "Elm Lang"
```

এবং স্ট্রিংয়ের লিস্টের জন্য, `String.join` আপনার বন্ধু:

```Elm
words : List String
words =
    ["Join", "the", "Elm", "club"]

sentence : String
sentence =
    String.join " " words

-- "Join the Elm club"
```

## গভীর ডাইভ
পুরনো দিনে, অন্য ভাষায় জটিল ফাংশনের মাধ্যমে প্রায়ই স্ট্রিং যুক্ত করা হতো। এলমে, এটি সবসময় একটি হাওয়া ছিল `(++)` অপারেটরের কারণে। যদি আপনি সত্যিই অনেক যুক্ত করছেন, দক্ষতা একটি ভূমিকা নিতে পারে; দীর্ঘ স্ট্রিংগুলির উপর `(++)` ব্যবহার করা ধীর হতে পারে, কারণ এলমকে `(++)` এর বাম দিকে পুরো স্ট্রিং জুড়ে প্রতিবারপ্রতি হাঁটতে হয়।

কিছু ভাষায় "ইন্টারপোলেশন" রয়েছে, তবে এলম স্ট্রিং ইন্টারপোলেশন করে না। তবে চিন্তা নেই, `(++)` এবং `String.join` আমাদের কাভার করে রেখেছে।

অন্তর্নিহিতভাবে, যখন এলম স্ট্রিং যুক্ত করে, এটি বুদ্ধিমান হতে চেষ্টা করে, প্রায়ই অনুকূলিত জাভাস্ক্রিপ্ট অপারেশনগুলি ব্যবহার করে, যা শেষ পর্যন্ত এলম কম্পাইল হয়। তাই যদিও `(++)` সহজ মনে হতে পারে, পেছনের দৃশ্যে কিছু চতুরতা চলমান আছে যা জিনিসগুলোকে দ্রুত রাখে।

## আরও দেখুন
- স্ট্রিং সম্পর্কে এলম অফিশিয়াল ডকুমেন্টেশন: https://package.elm-lang.org/packages/elm/core/latest/String
- এলম গাইড, যেখানে আপনি স্ট্ঁগ সম্পর্কে আরও শিখতে পারেন: https://guide.elm-lang.org/strings/

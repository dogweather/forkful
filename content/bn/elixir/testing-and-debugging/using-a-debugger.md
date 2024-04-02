---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:21:48.455154-06:00
description: "\u0987\u09B2\u09BF\u0995\u09CD\u09B8\u09BF\u09B0\u09C7 \u098F\u0995\u099F\
  \u09BF \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u0997\u09CD\u09B0\u09BE\u09AB\
  \u09BF\u0995\u09BE\u09B2 \u09A1\u09BF\u09AC\u09BE\u0997\u09BE\u09B0 `:debugger`\
  \ \u09B8\u09B9 \u0986\u09B8\u09C7\u0964 \u098F\u099F\u09BF \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\u09C7, \u0986\u09AA\u09A8\u09BE\u0995\u09C7\
  \ \u098F\u099F\u09BF \u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09C7 \u0986\u09AA\u09A8\
  \u09BE\u09B0 \u099A\u09B2\u09AE\u09BE\u09A8 \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\
  \u09BF\u09AF\u09BC\u09BE\u09A4\u09C7 \u09AF\u09C1\u0995\u09CD\u09A4 \u09B9\u09A4\
  \u09C7 \u09B9\u09AC\u09C7\u0964 \u09AA\u09CD\u09B0\u09A5\u09AE\u09C7,\u2026"
lastmod: '2024-03-17T18:47:43.674895-06:00'
model: gpt-4-0125-preview
summary: "\u0987\u09B2\u09BF\u0995\u09CD\u09B8\u09BF\u09B0\u09C7 \u098F\u0995\u099F\
  \u09BF \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u0997\u09CD\u09B0\u09BE\u09AB\
  \u09BF\u0995\u09BE\u09B2 \u09A1\u09BF\u09AC\u09BE\u0997\u09BE\u09B0 `:debugger`\
  \ \u09B8\u09B9 \u0986\u09B8\u09C7\u0964 \u098F\u099F\u09BF \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\u09C7, \u0986\u09AA\u09A8\u09BE\u0995\u09C7\
  \ \u098F\u099F\u09BF \u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09C7 \u0986\u09AA\u09A8\
  \u09BE\u09B0 \u099A\u09B2\u09AE\u09BE\u09A8 \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\
  \u09BF\u09AF\u09BC\u09BE\u09A4\u09C7 \u09AF\u09C1\u0995\u09CD\u09A4 \u09B9\u09A4\
  \u09C7 \u09B9\u09AC\u09C7\u0964 \u09AA\u09CD\u09B0\u09A5\u09AE\u09C7,\u2026"
title: "\u09A1\u09BF\u09AC\u09BE\u0997\u09BE\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09BE"
weight: 35
---

## কিভাবে:
ইলিক্সিরে একটি বিল্ট-ইন গ্রাফিকাল ডিবাগার `:debugger` সহ আসে। এটি ব্যবহার করতে, আপনাকে এটি শুরু করে আপনার চলমান প্রক্রিয়াতে যুক্ত হতে হবে।

প্রথমে, নিশ্চিত করুন আপনি `:debugger` কে একটি `iex` সেশনের মধ্যে শুরু করেছেন:
```elixir
iex> :debugger.start()
{:ok, #PID<0.108.0>}
```

এখন, আপনি যে কোড মডিউলটি ডিবাগ করতে চান তা ইন্টারপ্রেট করুন:
```elixir
iex> :int.ni(MyApp.MyModule)
{:module, MyApp.MyModule}
```

আপনি একটি ব্রেকপয়েন্ট সেট করতে পারেন:
```elixir
iex> :int.break(MyApp.MyModule, line_number)
:ok
```

এবং তারপর, আপনার ফাংশনটি চালান যাতে ব্রেকপয়েন্টে পৌঁছে আপনার কোডের মধ্য দিয়ে চলে যেতে পারেন:
```elixir
iex> MyApp.MyModule.my_function(arg1, arg2)
# ডিবাগার ব্রেকপয়েন্ট সহ লাইনে এক্সিকিউশন স্থগিত করবে
```

## গভীর ডুব
ইলিক্সিরের `:debugger` এর আগে, এরল্যাং ডিবাগারটি প্রদান করেছিল যা ইলিক্সির ব্যবহার করে; এটি সংহত প্রক্রিয়া সামাল দেওয়ার দিক থেকে দৃঢ় এবং দুর্দান্ত, যা এরল্যাং VM (BEAM) এর একটি মিষ্টি স্থান। অন্যান্য কিছু ডিবাগারের মতো, `:debugger` উপর উড়ে চলে যাওয়া ভেরিয়েবলগুলির পরিবর্তন অনুমতি দেয় না, ইলিক্সিরে তথ্যের অপরিবর্তনীয় প্রকৃতির কারণে। বিকল্প হিসাবে, আপনি `IEx.pry` আছে যা আপনাকে এক্সিকিউশন স্থগিত করতে এবং আপনার কোডের যেকোনো বিন্দুতে একটি REPL এ লাফ দিতে দেয়, যা খুবই সুবিধাজনক হতে পারে।

`ডিবাগার` একটি গ্রাফিকাল ইন্টারফেসের জন্য ভাল, কিছু লোক সম্ভবত প্রক্রিয়া পরিদর্শন এবং সিস্টেম মেট্রিকস অফার করে এমন বিল্ট-ইন `:observer` টুলটি পছন্দ করতে পারে, যদিও সেটি কোডের মধ্য দিয়ে যাওয়ার দিকে বিশেষভাবে লক্ষ্যিত নয়। ইলিক্সিরের কমিউনিটি `visualixir` এবং `rexbug` এর মতো টুলগুলি অবদান রাখে, ডিফল্টগুলির বাইরে ডিবাগ টুলগুলির ইকোসিস্টেম সম্প্রসারণ করে।

## আরো দেখুন
- ডিবাগিং সম্পর্কে অফিশিয়াল ইলিক্সির গেটিং স্টার্টেড গাইড: https://elixir-lang.org/getting-started/debugging.html
- এরল্যাংয়ের `:debugger` ডকুমেন্টেশন: http://erlang.org/doc/apps/debugger/debugger_chapter.html
- ডিবাগিং টেকনিক সম্পর্কে ইলিক্সির ফোরাম আলোচনা: https://elixirforum.com/c/elixir-questions/elixir-questions-questions-help/15

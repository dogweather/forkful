---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:20:38.433175-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u098F\u0995\u099F\u09BF \u09A8\
  \u09A4\u09C1\u09A8 \u09AA\u09CD\u09B0\u099C\u09C7\u0995\u09CD\u099F \u09A4\u09C8\
  \u09B0\u09BF \u0995\u09B0\u09A4\u09C7, `mix new` \u0995\u09AE\u09BE\u09A8\u09CD\u09A1\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C1\u09A8."
lastmod: '2024-03-17T18:47:43.670161-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u09A8\u09A4\u09C1\u09A8 \u09AA\u09CD\u09B0\u099C\
  \u09C7\u0995\u09CD\u099F \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09A4\u09C7, `mix\
  \ new` \u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09C1\u09A8."
title: "\u09A8\u09A4\u09C1\u09A8 \u09AA\u09CD\u09B0\u0995\u09B2\u09CD\u09AA \u09B6\
  \u09C1\u09B0\u09C1 \u0995\u09B0\u09BE"
weight: 1
---

## কিভাবে:
একটি নতুন প্রজেক্ট তৈরি করতে, `mix new` কমান্ড ব্যবহার করুন:

```elixir
$ mix new my_app
```

আপনি এমন কিছু দেখতে পাবেন:

```
* creating README.md
* creating .formatter.exs
* creating .gitignore
* creating mix.exs
* creating lib
* creating lib/my_app.ex
* creating test
* creating test/test_helper.exs
* creating test/my_app_test.exs
```

এবার আপনার নতুন প্রজেক্ট ডিরেক্টরির মধ্যে যান:

```elixir
$ cd my_app
```

এখন, আপনি আপনার প্রজেক্ট বা এর টেস্ট চালাতে পারেন:

আপনার প্রজেক্ট চালান:

```elixir
$ iex -S mix
```
টেস্ট করুন:

```elixir
$ mix test
```

## গভীরে ডুব দেয়া
Elixir- এর বিল্ড টুল, Mix, প্রজেক্ট তৈরি, কনফিগার করা, এবং ম্যানেজ করার একটি নির্ভরযোগ্য ও ঐক্যবদ্ধ উপায় প্রদান করার ইচ্ছা থেকে এসেছে। এটি অন্যান্য ইকোসিস্টেমের টুলস থেকে প্রভাবিত, যেমন রুবির বান্ডলার এবং রেক। Mix এ এলিক্সিরের টুলবেল্টে নির্ভরতা পরিচালনা এবং টাস্ক অটোমেশন নিয়ে আসে। এর অন্যান্য ভাষায় বিকল্প হতে পারে npm Node.js এর জন্য অথবা Maven Java এর জন্য। তবে, Mix এলিক্সির রানটাইমের সাথে মানানসই এবং তার স্বাভাবিক প্যাটার্নগুলির সাথে ইন্টিগ্রেটেড। `mix new` কমান্ডটি পূর্বনির্ধারিত ডিরেক্টরিগুলি এবং ফাইলগুলি সহ একটি প্রথাগত কাঠামো, যেমন কনফিগারেশন ফাইল, মডিউল ডেফিনিশন, এবং টেস্ট স্যুট, তৈরি করে। Elixir-এ প্রথাগুলি অনুসরণ করা মুখ্য; এটি এলিক্সির প্রজেক্টগুলি জুড়ে কোডের ধারাবাহিকতা এবং পঠনযোগ্যতা উৎসাহিত করে।

## আরও দেখুন
- অফিসিয়াল `mix` ডকুমেন্টেশন: [https://hexdocs.pm/mix/Mix.html](https://hexdocs.pm/mix/Mix.html)
- এলিক্সির স্কুলের প্রজেক্ট গাইড: [https://elixirschool.com/en/lessons/basics/mix/](https://elixirschool.com/en/lessons/basics/mix/)

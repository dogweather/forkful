---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:20:38.433175-06:00
description: "\u09A8\u09A4\u09C1\u09A8 \u098F\u0995\u099F\u09BF Elixir \u09AA\u09CD\
  \u09B0\u099C\u09C7\u0995\u09CD\u099F \u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09BE\
  \ \u09AE\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09A8\u09A4\u09C1\u09A8 Mix\
  \ \u09AA\u09CD\u09B0\u099C\u09C7\u0995\u09CD\u099F \u0995\u09BE\u09A0\u09BE\u09AE\
  \u09CB \u09B8\u09C7\u099F \u0986\u09AA \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF\
  \ \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u09A8 \u09AF\u09BE\u09A4\u09C7 \u09A4\
  \u09BE\u09B0\u09BE \u09B8\u09C1\u09B8\u0982\u0997\u09A0\u09BF\u09A4 \u0995\u09CB\
  \u09A1 \u098F\u09AC\u0982 \u0985\u099F\u09CB\u09AE\u09C7\u099F\u09C7\u09A1 \u099F\
  \u09C1\u09B2\u09BF\u0982\u2026"
lastmod: '2024-03-17T18:47:43.670161-06:00'
model: gpt-4-0125-preview
summary: "\u09A8\u09A4\u09C1\u09A8 \u098F\u0995\u099F\u09BF Elixir \u09AA\u09CD\u09B0\
  \u099C\u09C7\u0995\u09CD\u099F \u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09BE \u09AE\
  \u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09A8\u09A4\u09C1\u09A8 Mix \u09AA\u09CD\
  \u09B0\u099C\u09C7\u0995\u09CD\u099F \u0995\u09BE\u09A0\u09BE\u09AE\u09CB \u09B8\
  \u09C7\u099F \u0986\u09AA \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\
  \u09C7 \u09A5\u09BE\u0995\u09C7\u09A8 \u09AF\u09BE\u09A4\u09C7 \u09A4\u09BE\u09B0\
  \u09BE \u09B8\u09C1\u09B8\u0982\u0997\u09A0\u09BF\u09A4 \u0995\u09CB\u09A1 \u098F\
  \u09AC\u0982 \u0985\u099F\u09CB\u09AE\u09C7\u099F\u09C7\u09A1 \u099F\u09C1\u09B2\
  \u09BF\u0982\u2026"
title: "\u09A8\u09A4\u09C1\u09A8 \u09AA\u09CD\u09B0\u0995\u09B2\u09CD\u09AA \u09B6\
  \u09C1\u09B0\u09C1 \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
নতুন একটি Elixir প্রজেক্ট শুরু করা মানে একটি নতুন Mix প্রজেক্ট কাঠামো সেট আপ করা। প্রোগ্রামাররা এটি করে থাকেন যাতে তারা সুসংগঠিত কোড এবং অটোমেটেড টুলিং দিয়ে শুরু করতে পারে, যা তাদের এলিক্সির অ্যাপ্লিকেশনগুলি দক্ষতার সাথে নির্মাণ ও পরীক্ষা করার কাজে সাহায্য করে।

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

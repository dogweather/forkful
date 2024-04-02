---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:09:48.686303-06:00
description: "\u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\
  \u09B0\u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F \u09AA\u09A1\u09BC\u09BE\
  \ \u098F\u0995\u099F\u09BF \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u0995\u09C7 \u099F\u09BE\u09B0\u09CD\u09AE\u09BF\u09A8\u09BE\u09B2 \u09A5\u09C7\
  \u0995\u09C7 \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF \u09A1\u09C7\u099F\u09BE \u09A8\
  \u09C7\u0993\u09AF\u09BC\u09BE\u09B0 \u0985\u09A8\u09C1\u09AE\u09A4\u09BF \u09A6\
  \u09C7\u09AF\u09BC\u2014\u09AF\u09C7\u09AE\u09A8 \u09B8\u09C7\u099F\u09BF\u0982\u09B8\
  \ \u09AC\u09BE \u09AB\u09BE\u0987\u09B2\u09C7\u09B0 \u09A8\u09BE\u09AE\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u0995\u09CB\
  \u09A1 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u09A8\u09BE \u0995\u09B0\
  \u09C7\u2026"
lastmod: '2024-03-17T18:47:43.687047-06:00'
model: gpt-4-0125-preview
summary: "\u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\u09B0\
  \u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F \u09AA\u09A1\u09BC\u09BE \u098F\
  \u0995\u099F\u09BF \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u0995\u09C7\
  \ \u099F\u09BE\u09B0\u09CD\u09AE\u09BF\u09A8\u09BE\u09B2 \u09A5\u09C7\u0995\u09C7\
  \ \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF \u09A1\u09C7\u099F\u09BE \u09A8\u09C7\u0993\
  \u09AF\u09BC\u09BE\u09B0 \u0985\u09A8\u09C1\u09AE\u09A4\u09BF \u09A6\u09C7\u09AF\
  \u09BC\u2014\u09AF\u09C7\u09AE\u09A8 \u09B8\u09C7\u099F\u09BF\u0982\u09B8 \u09AC\
  \u09BE \u09AB\u09BE\u0987\u09B2\u09C7\u09B0 \u09A8\u09BE\u09AE\u0964 \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u0995\u09CB\u09A1\
  \ \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u09A8\u09BE \u0995\u09B0\u09C7\
  \u2026"
title: "\u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\u09B0\
  \u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F\u0997\u09C1\u09B2\u09BF \u09AA\u09A1\
  \u09BC\u09BE"
weight: 23
---

## কি এবং কেন?

কমান্ড লাইন আর্গুমেন্ট পড়া একটি প্রোগ্রামকে টার্মিনাল থেকে সরাসরি ডেটা নেওয়ার অনুমতি দেয়—যেমন সেটিংস বা ফাইলের নাম। প্রোগ্রামাররা কোড পরিবর্তন না করে একটি প্রোগ্রামের আচরণ কাস্টমাইজ করার জন্য এটি করে থাকেন।

## কিভাবে:

Elixir এ, কমান্ড লাইন আর্গুমেন্ট ধরা বেশ সহজ। `System.argv()` ব্যবহার করুন এবং আপনি তাদের স্ট্রিংগুলোর একটি তালিকা হিসাবে পেয়ে যাবেন।

```elixir
defmodule CliArgs do
  def main do
    args = System.argv()
    IO.inspect(args)
  end
end

CliArgs.main()
```

`elixir cli_args.exs foo bar baz` এর মত রান করুন, এবং আশা করুন:

```
["foo", "bar", "baz"]
```

আপনি আর্গুমেন্ট `foo`, `bar`, এবং `baz` কে সেখানে একটি Elixir তালিকায় দেখছেন।

## গভীরে গমন

ঐতিহাসিকভাবে, কমান্ড লাইন আর্গুমেন্ট পুরানো পাহাড়ের মতো, প্রাথমিক CLI পরিবেশ থেকে এর উৎপত্তি। Elixir এ, `System.argv()` এই কাজের জন্য আপনার বিশ্বস্ত ঘোড়া। কেন? কারণ এটি Erlang VM এ সরাসরি অন্তর্ভুক্ত, যেটির উপর Elixir তৈরি।

বিকল্প? অবশ্যই, আপনার কাছে আর্গুমেন্ট পার্স করে এমন লাইব্রেরি রয়েছে, যা ফ্ল্যাগ এবং অপশন যোগ করে। কিন্তু ভ্যানিলা Elixir এর জন্য, `System.argv()` একমাত্র পথ।

বাস্তবায়নের দিক থেকে, মনে রাখা গুরুত্বপূর্ণ যে `System.argv()` আপনাকে সমস্ত আর্গুমেন্ট স্ট্রিং হিসাবে দেয়। যদি আপনার সংখ্যা বা অন্য ধরণের প্রয়োজন হয়, তাহলে আপনাকে তাদের ম্যানুয়ালি রূপান্তর করতে হবে। তাছাড়া, অর্ডারের বিষয়টি গুরুত্বপূর্ণ। আপনার প্রথম কমান্ড লাইন আর্গুমেন্ট হল `List.first(System.argv())`, এবং তার পর যথাক্রমে।

## আরও দেখুন

আরও জানতে:
- [Elixir's System module docs](https://hexdocs.pm/elixir/System.html) অন্যান্য হ্যান্ডি সিস্টেম-সম্পর্কিত ফাংশনগুলির জন্য।
- [Optparse](https://hexdocs.pm/elixir/OptionParser.html) Elixir এর স্ট্যান্ডার্ড লাইব্রেরিতে, যা কমান্ড লাইন অপশন পার্স করার জন্য একটি দানব।
- [Erlang's init docs](http://erlang.org/doc/man/init.html) যদি আপনি Elixir কে সাপোর্ট করা VM এর আন্ডার-দ্য-হুড ম্যাজিক সম্পর্কে কৌতূহলী হন।

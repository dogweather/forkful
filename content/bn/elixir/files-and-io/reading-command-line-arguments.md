---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:09:48.686303-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Elixir \u098F, \u0995\u09AE\u09BE\
  \u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\u09B0\u09CD\u0997\u09C1\u09AE\
  \u09C7\u09A8\u09CD\u099F \u09A7\u09B0\u09BE \u09AC\u09C7\u09B6 \u09B8\u09B9\u099C\
  \u0964 `System.argv()` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C1\
  \u09A8 \u098F\u09AC\u0982 \u0986\u09AA\u09A8\u09BF \u09A4\u09BE\u09A6\u09C7\u09B0\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0997\u09C1\u09B2\u09CB\u09B0 \u098F\
  \u0995\u099F\u09BF \u09A4\u09BE\u09B2\u09BF\u0995\u09BE \u09B9\u09BF\u09B8\u09BE\
  \u09AC\u09C7 \u09AA\u09C7\u09DF\u09C7 \u09AF\u09BE\u09AC\u09C7\u09A8\u0964."
lastmod: '2024-03-17T18:47:43.687047-06:00'
model: gpt-4-0125-preview
summary: "Elixir \u098F, \u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8\
  \ \u0986\u09B0\u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F \u09A7\u09B0\u09BE\
  \ \u09AC\u09C7\u09B6 \u09B8\u09B9\u099C\u0964 `System.argv()` \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C1\u09A8 \u098F\u09AC\u0982 \u0986\u09AA\
  \u09A8\u09BF \u09A4\u09BE\u09A6\u09C7\u09B0 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982\u0997\u09C1\u09B2\u09CB\u09B0 \u098F\u0995\u099F\u09BF \u09A4\u09BE\u09B2\
  \u09BF\u0995\u09BE \u09B9\u09BF\u09B8\u09BE\u09AC\u09C7 \u09AA\u09C7\u09DF\u09C7\
  \ \u09AF\u09BE\u09AC\u09C7\u09A8\u0964."
title: "\u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\u09B0\
  \u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F\u0997\u09C1\u09B2\u09BF \u09AA\u09A1\
  \u09BC\u09BE"
weight: 23
---

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

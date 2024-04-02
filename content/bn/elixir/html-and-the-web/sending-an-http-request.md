---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:17:46.616652-06:00
description: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A8\
  \u09CB \u09B9\u099A\u09CD\u099B\u09C7 \u0986\u09AA\u09A8\u09BE\u09B0 \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\
  \u09AE\u09C7 \u0993\u09AF\u09BC\u09C7\u09AC \u09A5\u09C7\u0995\u09C7 \u09A1\u09BE\
  \u099F\u09BE \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u0995\u09B0\u09BE, \u09AA\u09CD\
  \u09B0\u09BE\u09AF\u09BC \u09AF\u09C7\u09AE\u09A8\u099F\u09BE \u0986\u09AA\u09A8\
  \u09BF \u098F\u0995\u099C\u09A8 \u0997\u09CD\u09B0\u09A8\u09CD\u09A5\u09BE\u0997\
  \u09BE\u09B0\u09BF\u0995\u09C7\u09B0 \u0995\u09BE\u099B\u09C7 \u098F\u0995\u099F\
  \u09BF \u09AC\u0987 \u099A\u09BE\u0987\u09AC\u09C7\u09A8\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\u2026"
lastmod: '2024-03-17T18:47:43.665075-06:00'
model: gpt-4-0125-preview
summary: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A8\
  \u09CB \u09B9\u099A\u09CD\u099B\u09C7 \u0986\u09AA\u09A8\u09BE\u09B0 \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\
  \u09AE\u09C7 \u0993\u09AF\u09BC\u09C7\u09AC \u09A5\u09C7\u0995\u09C7 \u09A1\u09BE\
  \u099F\u09BE \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u0995\u09B0\u09BE, \u09AA\u09CD\
  \u09B0\u09BE\u09AF\u09BC \u09AF\u09C7\u09AE\u09A8\u099F\u09BE \u0986\u09AA\u09A8\
  \u09BF \u098F\u0995\u099C\u09A8 \u0997\u09CD\u09B0\u09A8\u09CD\u09A5\u09BE\u0997\
  \u09BE\u09B0\u09BF\u0995\u09C7\u09B0 \u0995\u09BE\u099B\u09C7 \u098F\u0995\u099F\
  \u09BF \u09AC\u0987 \u099A\u09BE\u0987\u09AC\u09C7\u09A8\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\u2026"
title: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3\
  \ \u0995\u09B0\u09BE"
weight: 44
---

## কি এবং কেন?

HTTP অনুরোধ পাঠানো হচ্ছে আপনার প্রোগ্রামের মাধ্যমে ওয়েব থেকে ডাটা অনুরোধ করা, প্রায় যেমনটা আপনি একজন গ্রন্থাগারিকের কাছে একটি বই চাইবেন। প্রোগ্রামাররা এটি করে থাকে দূরবর্তী ডাটা আনা, পাঠানো বা ম্যানিপুলেট করার জন্য, আবহাওয়া জানা থেকে টুইট পোস্ট করা পর্যন্ত।

## কিভাবে:

Elixir-এর `HTTPoison` লাইব্রেরি ব্যবহার করুন। এটা পরিষ্কার, সহজ এবং কাজটি সঠিকভাবে করে।

1. আপনার প্রজেক্টের `mix.exs`-এ HTTPoison যোগ করুন:

```elixir
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end
```

2. নির্ভরতা আনার জন্য টার্মিনালে `mix deps.get` চালান।

3. এখন আপনার GET অনুরোধ পাঠানোর জন্য তৈরি:

```elixir
case HTTPoison.get("https://jsonplaceholder.typicode.com/posts/1") do
  {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
    IO.inspect(body) # আপনি আপনার ডাটা পেয়ে গেছেন!
  {:error, %HTTPoison.Error{reason: reason}} ->
    IO.inspect(reason) # ত্রুটি সামলান
end
```

নমুনা আউটপুট: প্লেসহোল্ডার API থেকে পোস্ট ডাটার একটি JSON স্ট্রিং।

## গভীর ডুব

ঐতিহাসিকভাবে, আপনি Erlang/OTP সাথে আসা `:httpc` বা Elixir-এর `HTTPotion` ব্যবহার করতেন। HTTPoison এখন আরও জনপ্রিয়, পরিষ্কার সিনট্যাক্স সহ এবং Hackney উপর নির্মিত, একটি শক্তিশালী HTTP ক্লায়েন্ট Erlang এর জন্য।

HTTPoison-এর বিকল্পে রয়েছে Tesla – মিডলওয়্যার সমর্থন সহ একটি নমনীয় HTTP ক্লায়েন্ট, এবং Mint – একটি ঝকঝকে, নিম্ন-স্তরের HTTP ক্লায়েন্ট।

বাস্তবায়নের দিক থেকে, এই লাইব্রেরিগুলি কানেকশন পুলিং, SSL, এবং কিপ-অ্যালাইভ ম্যানেজ করে, যা HTTP অনুরোধগুলি দক্ষ করার জন্য জটিল কিন্তু অপরিহার্য বিষয়। তারা যেন বন্ধুবৎসল গ্রন্থাগারিক যারা দুরুহ কাজগুলি সামলান, যাতে আপনার নিজেকে সেলফের মধ্যে দিয়ে কাজ করতে না হয়।

## আরও দেখুন
- [HTTPoison GitHub](https://github.com/edgurgel/httpoison) – সব বিস্তারিত এবং আপডেটের জন্য।
- [HTTPoison-এর HexDocs](https://hexdocs.pm/httpoison) – ব্যাপক ডকুমেন্টেশনের জায়গা।
- [Elixir ফোরাম](https://elixirforum.com) – কমিউনিটির সাথে আলাপ করার জায়গা।

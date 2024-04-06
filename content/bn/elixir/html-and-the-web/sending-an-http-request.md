---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:17:46.616652-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Elixir-\u098F\u09B0 `HTTPoison`\
  \ \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09C1\u09A8\u0964 \u098F\u099F\u09BE \u09AA\u09B0\
  \u09BF\u09B7\u09CD\u0995\u09BE\u09B0, \u09B8\u09B9\u099C \u098F\u09AC\u0982 \u0995\
  \u09BE\u099C\u099F\u09BF \u09B8\u09A0\u09BF\u0995\u09AD\u09BE\u09AC\u09C7 \u0995\
  \u09B0\u09C7\u0964 1. \u0986\u09AA\u09A8\u09BE\u09B0 \u09AA\u09CD\u09B0\u099C\u09C7\
  \u0995\u09CD\u099F\u09C7\u09B0 `mix.exs`-\u098F HTTPoison \u09AF\u09CB\u0997 \u0995\
  \u09B0\u09C1\u09A8."
lastmod: '2024-04-05T21:53:51.734836-06:00'
model: gpt-4-0125-preview
summary: "Elixir-\u098F\u09B0 `HTTPoison` \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\
  \u09B0\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C1\u09A8\
  \u0964 \u098F\u099F\u09BE \u09AA\u09B0\u09BF\u09B7\u09CD\u0995\u09BE\u09B0, \u09B8\
  \u09B9\u099C \u098F\u09AC\u0982 \u0995\u09BE\u099C\u099F\u09BF \u09B8\u09A0\u09BF\
  \u0995\u09AD\u09BE\u09AC\u09C7 \u0995\u09B0\u09C7\u0964 1."
title: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3\
  \ \u0995\u09B0\u09BE"
weight: 44
---

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

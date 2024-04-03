---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:38:45.846729-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Elixir-\u098F \u09AC\u09BF\u09B2\
  \u09CD\u099F-\u0987\u09A8 \u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\
  \u09BE \u09A8\u09C7\u0987, \u09A4\u09BE\u0987 \u0986\u09AE\u09B0\u09BE \u09A8\u09BF\
  \u099C\u09C7\u09B0\u09BE \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BF \u0985\u09A5\
  \u09AC\u09BE \u098F\u0995\u099F\u09BF \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\
  \u09B0\u09BF, \u09AF\u09C7\u09AE\u09A8 `ComplexNum`-\u098F\u09B0 \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BF\u0964 \u098F\u0996\u09BE\u09A8\u09C7\
  \ \u098F\u0995\u099F\u09BF \u09B2\u09BE\u0987\u09AC\u09C7\u09B0 \u09B8\u09BE\u09A5\
  \u09C7 \u098F\u0995\u099F\u09BF\u2026"
lastmod: '2024-03-17T18:47:43.661728-06:00'
model: gpt-4-0125-preview
summary: "Elixir-\u098F \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u099C\u099F\u09BF\
  \u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09A8\u09C7\u0987, \u09A4\u09BE\u0987\
  \ \u0986\u09AE\u09B0\u09BE \u09A8\u09BF\u099C\u09C7\u09B0\u09BE \u09A4\u09C8\u09B0\
  \u09BF \u0995\u09B0\u09BF \u0985\u09A5\u09AC\u09BE \u098F\u0995\u099F\u09BF \u09B2\
  \u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF, \u09AF\u09C7\u09AE\u09A8 `ComplexNum`-\u098F\
  \u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BF\u0964 \u098F\
  \u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09B2\u09BE\u0987\u09AC\u09C7\
  \u09B0 \u09B8\u09BE\u09A5\u09C7 \u098F\u0995\u099F\u09BF \u09A6\u09CD\u09B0\u09C1\
  \u09A4 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09DF\u09BE \u09B9\
  \u09B2."
title: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 14
---

## কিভাবে:
Elixir-এ বিল্ট-ইন জটিল সংখ্যা নেই, তাই আমরা নিজেরা তৈরি করি অথবা একটি লাইব্রেরি, যেমন `ComplexNum`-এর ব্যবহার করি। এখানে একটি লাইবের সাথে একটি দ্রুত উদাহরণ দেওয়া হল:

```elixir
# ধরে নিচ্ছি আপনার কাছে ComplexNum ইন্সটল করা আছে
defmodule ComplexMath do
  import ComplexNum

  def add(a, b) do
    ComplexNum.add(a, b)
  end
end

# জটিল সংখ্যা তৈরি করুন এবং যোগ করুন
c1 = {3, 4}   # 3 + 4i কে প্রকাশ করে
c2 = {2, -3}  # 2 - 3i কে প্রকাশ করে
result = ComplexMath.add(c1, c2)
IO.puts "ফলাফল হল: #{inspect(result)}"
```

এটি আউটপুট করবে:
```
ফলাফল হল: {5, 1}
```

এর অর্থ `3 + 4i` এবং `2 - 3i`-এর যোগফল হল `5 + 1i`।

## গভীর ডুব
ইতিহাসে জটিল সংখ্যাগুলি উপস্থিত হয়েছিল কারণ সাধারণ সংখ্যাগুলি ঋণাত্মকগুলির বর্গমূল সম্ভাল করতে পারেনি। ১৭ শতাব্দীতে আসার পরে তাদের গুরুত্ব দেওয়া শুরু হয়েছিল, রেনে দেকার্ত এবং গেরোলামো কার্দানোর মতো গণিতবিদদের কল্যাণে।

Elixir-এ, আপনি প্রায়শই `{3, 4}` এর মত টাপল ব্যবহার করেন জটিল সংখ্যা হিসেবে, অথবা চাকাটি পুনরায় আবিষ্কার করা এড়াতে একটি নির্দিষ্ট লাইব ব্যবহার করেন। লাইব্রেরিগুলি সাধারণত ভালো—এগুলি কাল্পনিক ইউনিট 'i' (FYI: `i`-এর বর্গ সমান `-1`) এর কারণে জটিল হয়ে ওঠা গুণিতক ও ভাগের মত জটিল বিষয়গুলি আলাদা করে।

## দেখুন ও
এই রিসোর্সগুলি দেখুন:
- [ComplexNum Library](https://hex.pm/packages/complex_num) Elixir-এর প্যাকেজ ম্যানেজার, Hex এর জন্য।
- [Elixir School](https://elixirschool.com/en/), উন্নত Elixir বিষয়ে এবং অনুশীলনের জন্য।
- [Erlang -- math Module](http://erlang.org/doc/man/math.html), যা Elixir ভিত্তির নীচে ব্যবহার করে, অন্যান্য গাণিতিক প্রয়োজনের জন্য।

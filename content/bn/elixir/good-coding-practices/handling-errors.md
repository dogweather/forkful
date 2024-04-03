---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:00.055422-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Elixir-\u098F, \u0986\u09AE\u09B0\
  \u09BE \u09AA\u09CD\u09B0\u09BE\u09DF\u09B6\u0987 \u09AC\u09BF\u09AD\u09BF\u09A8\
  \u09CD\u09A8 \u09AB\u09B2\u09BE\u09AB\u09B2\u0997\u09C1\u09B2\u09BF, \u09A4\u09CD\
  \u09B0\u09C1\u099F\u09BF\u0997\u09C1\u09B2\u09BF \u09B8\u09B9, \u09B8\u09BE\u09AE\
  \u09BE\u09B2 \u09A6\u09BF\u09A4\u09C7 \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\
  \u09CD\u09A8 \u09AE\u09CD\u09AF\u09BE\u099A\u09BF\u0982 \u098F\u09AC\u0982 `case`\
  \ \u09B8\u09CD\u099F\u09C7\u099F\u09AE\u09C7\u09A8\u09CD\u099F \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BF\u0964."
lastmod: '2024-03-17T18:47:43.678405-06:00'
model: gpt-4-0125-preview
summary: "Elixir-\u098F, \u0986\u09AE\u09B0\u09BE \u09AA\u09CD\u09B0\u09BE\u09DF\u09B6\
  \u0987 \u09AC\u09BF\u09AD\u09BF\u09A8\u09CD\u09A8 \u09AB\u09B2\u09BE\u09AB\u09B2\
  \u0997\u09C1\u09B2\u09BF, \u09A4\u09CD\u09B0\u09C1\u099F\u09BF\u0997\u09C1\u09B2\
  \u09BF \u09B8\u09B9, \u09B8\u09BE\u09AE\u09BE\u09B2 \u09A6\u09BF\u09A4\u09C7 \u09AA\
  \u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8 \u09AE\u09CD\u09AF\u09BE\u099A\u09BF\
  \u0982 \u098F\u09AC\u0982 `case` \u09B8\u09CD\u099F\u09C7\u099F\u09AE\u09C7\u09A8\
  \u09CD\u099F \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BF\u0964\
  ."
title: "\u09A4\u09CD\u09B0\u09C1\u099F\u09BF\u0997\u09C1\u09B2\u09BF \u09AA\u09B0\u09BF\
  \u099A\u09BE\u09B2\u09A8\u09BE \u0995\u09B0\u09BE"
weight: 16
---

## কিভাবে:
Elixir-এ, আমরা প্রায়শই বিভিন্ন ফলাফলগুলি, ত্রুটিগুলি সহ, সামাল দিতে প্যাটার্ন ম্যাচিং এবং `case` স্টেটমেন্ট ব্যবহার করি।

```elixir
defmodule Example do
  def divide(a, b) do
    case b do
      0 -> {:error, "শূন্য দ্বারা ভাগ করা যাবে না।"}
      _ -> {:ok, a / b}
    end
  end
end

# সফল ভাগফল
{:ok, result} = Example.divide(10, 2)
IO.puts("10 / 2 is #{result}")

# শূন্য দ্বারা ভাগ করার চেষ্টা
{:error, reason} = Example.divide(10, 0)
IO.puts("ত্রুটি: #{reason}")
```

নমুনা আউটপুট:
```
10 / 2 is 5.0
ত্রুটি: শূন্য দ্বারা ভাগ করা যাবে না।
```

আপনি যখন এই Elixir কোডটি চালাবেন, তখন আপনার ইনপুটের উপর ভিত্তি করে আপনি একটি সফল ভাগফল বা একটি ত্রুটি বার্তা পাবেন। এখানে কোন ক্র্যাশ নেই!

## গভীর ডুব
অনেক আগে, ত্রুটি সামালনের অর্থ প্রায়ই রিটার্ন মানগুলি চেক করা হতো। তবে Elixir-এর ফাংশনাল মূলের দিকে তাকালে, আমাদের প্যাটার্ন ম্যাচিং এবং ট্যাগড টাপলস রয়েছে, যেমন `{:ok, value}` বা `{:error, reason}`, যা আরও সুস্থ্য।

Elixir-এ ত্রুটিগুলি সামাল দেওয়ার অন্যান্য উপায়গুলি:

- **Elixir-এর `try` এবং `rescue`** যা প্রাথমিক ভাষাগুলিতে `try-catch`-এর মতো কিন্তু Elixir-এর স্পষ্টতার প্রেফারেন্সের কারণে কম ব্যবহৃত হয়।
- **Supervisors and GenServers**, Elixir-এর OTP ফ্রেমওয়ার্কের অংশ, যা আরো বেশি ত্রুটি সহিষ্ণুতা সম্পর্কে। তারা আপনার কোডের প্রসেসকে নজর রাখে, জিনিসগুলি ভুল হলে আবার চালু করার জন্য প্রস্তুত।

বাস্তবায়নের দিক থেকে, Elixir এরল্যাং-এর দৃঢ়তা উপর নির্মিত। এটি ত্রুটিগুলিকে সামলানোর জন্য আরেক ধরণের বার্তা হিসেবে বিবেচনা করে, সমস্ত প্যাটার্ন ম্যাচিং এবং ফাংশনাল ভালোবাসার সাথে।

## দেখুন আরও
Elixir-এ ত্রুটি সামালনের উপর আরো পড়ার জন্য:

- Elixir-এর অফিসিয়াল গাইডে ত্রুটি সামালনে [তথ্য পান](https://elixir-lang.org/getting-started/try-catch-and-rescue.html)।
- [প্রসেস এবং OTP](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html) সম্পর্কে আরও জানুন।
- Elixir ফোরাম সবসময় প্রশ্ন জিজ্ঞাসা করার জন্য একটি ভালো জায়গা: [https://elixirforum.com](https://elixirforum.com)।

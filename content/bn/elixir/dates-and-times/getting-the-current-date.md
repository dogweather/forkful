---
changelog:
- 2024-02-05, dogweather, reviewed and corrected
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:48.044527-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Elixir-\u098F\u09B0 \u09B8\u09CD\
  \u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u09B2\u09BE\u0987\
  \u09AC\u09CD\u09B0\u09C7\u09B0\u09BF, `DateTime` \u09AE\u09A1\u09BF\u0989\u09B2\u09C7\
  \u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7, \u09AC\u09B0\u09CD\u09A4\u09AE\
  \u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996 \u098F\u09AC\u0982 \u09B8\u09AE\u09AF\
  \u09BC \u0986\u09A8\u09BE\u09B0 \u09B8\u09C1\u09AF\u09CB\u0997 \u09A6\u09C7\u09AF\
  \u09BC\u0964 \u09AF\u09C7\u09B9\u09C7\u09A4\u09C1 Elixir \u098F\u09B0\u09B2\u09CD\
  \u09AF\u09BE\u0982 VM (BEAM)-\u098F \u099A\u09B2\u09C7, \u098F\u099F\u09BF\u2026"
lastmod: '2024-04-05T21:53:51.766534-06:00'
model: gpt-4-0125-preview
summary: "Elixir-\u098F\u09B0 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\
  \u09BE\u09B0\u09CD\u09A1 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\
  , `DateTime` \u09AE\u09A1\u09BF\u0989\u09B2\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\
  \u09AF\u09AE\u09C7, \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\
  \u09BF\u0996 \u098F\u09AC\u0982 \u09B8\u09AE\u09AF\u09BC \u0986\u09A8\u09BE\u09B0\
  \ \u09B8\u09C1\u09AF\u09CB\u0997 \u09A6\u09C7\u09AF\u09BC\u0964 \u09AF\u09C7\u09B9\
  \u09C7\u09A4\u09C1 Elixir \u098F\u09B0\u09B2\u09CD\u09AF\u09BE\u0982 VM (BEAM)-\u098F\
  \ \u099A\u09B2\u09C7, \u098F\u099F\u09BF \u09B8\u09AE\u09AF\u09BC \u09B8\u09AE\u09CD\
  \u09AA\u09B0\u09CD\u0995\u09BF\u09A4 \u0985\u09AA\u09BE\u09B0\u09C7\u09B6\u09A8\u0997\
  \u09C1\u09B2\u09BF\u09B0 \u099C\u09A8\u09CD\u09AF \u0985\u09A8\u09CD\u09A4\u09B0\
  \u09CD\u09A8\u09BF\u09B9\u09BF\u09A4 \u098F\u09B0\u09B2\u09CD\u09AF\u09BE\u0982\
  \ \u09AB\u09BE\u0982\u09B6\u09A8\u09BE\u09B2\u09BF\u099F\u09BF\u0997\u09C1\u09B2\
  \u09BF\u0995\u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \u0964."
title: "\u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u09AA\u09C7\u09A4\u09C7"
weight: 29
---

## কিভাবে:
Elixir-এর স্ট্যান্ডার্ড লাইব্রেরি, `DateTime` মডিউলের মাধ্যমে, বর্তমান তারিখ এবং সময় আনার সুযোগ দেয়। যেহেতু Elixir এরল্যাং VM (BEAM)-এ চলে, এটি সময় সম্পর্কিত অপারেশনগুলির জন্য অন্তর্নিহিত এরল্যাং ফাংশনালিটিগুলিকে ব্যবহার করে।

### Elixir-এর স্ট্যান্ডার্ড লাইব্রেরি ব্যবহার করে
Elixir বর্তমান তারিখ এবং সময় UTC-তে পেতে `DateTime.utc_now/0` ফাংশনটি প্রদান করে।

```elixir
current_datetime_utc = DateTime.utc_now()
IO.inspect(current_datetime_utc)
```

**নমুনা আউটপুট:**
```
~U[2024-02-05 19:58:40.925931Z]
```

শুধুমাত্র বর্তমান তারিখ পেতে, আপনি বছর, মাস, এবং দিনের উপাদানগুলি বের করতে পারেন:

```elixir
{:ok, current_date} = Date.new(current_datetime_utc.year, current_datetime_utc.month, current_datetime_utc.day)
IO.inspect(current_date)
```

**নমুনা আউটপুট:**
```
~D[2023-05-04]
```

### Timex লাইব্রেরি ব্যবহার করে
আরও জটিল তারিখ-সময় প্রয়োজনের জন্য, Timex নামক একটি জনপ্রিয় থার্ড-পার্টি লাইব্রেরি ব্যবহার করা যেতে পারে। প্রথমে, আপনার mix.exs নির্ভরতাগুলিতে `Timex` যুক্ত করুন:

```elixir
defp deps do
  [
    {:timex, "~> 3.7"}
  ]
end
```

নির্ভরতা ইনস্টল করার পর (`mix deps.get`), আপনি Timex ব্যবহার করে বর্তমান তারিখ পেতে পারেন:

```elixir
current_date = Timex.today()
IO.inspect(current_date)
```

**নমুনা আউটপুট:**
```
~D[2023-05-04]
```

Timex তারিখ-সময় পরিচালনার জন্য ব্যাপক ফাংশনালিটি প্রদান করে, যা সময় অঞ্চল, ফরম্যাটিং, এবং তারিখ এবং সময়ের পার্সিং নিয়ে কাজ করার সময় Elixir অ্যাপ্লিকেশনগুলির জন্য একটি শক্তিশালী সংযোজন করে।

Elixir-এর বিল্ট-ইন ক্ষমতাগুলি এবং Timex লাইব্রেরি বুঝতে এবং ব্যবহার করে, আপনি আপনার Elixir অ্যাপ্লিকেশনগুলিতে তারিখ এবং সময় নিয়ে সহজেই কাজ করতে পারবেন, আপনার অ্যাপ্লিকেশনের প্রয়োজনীয়তাগুলি অনুযায়ী অভিজ্ঞতাকে নির্দিষ্ট এবং সহজ করে তুলতে।

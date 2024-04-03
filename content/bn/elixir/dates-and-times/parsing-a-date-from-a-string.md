---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:06:03.322060-06:00
description: "\u0995\u09C0\u09AD\u09BE\u09AC\u09C7: Elixir-\u098F\u09B0 \u09B8\u09CD\
  \u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u09B2\u09BE\u0987\
  \u09AC\u09CD\u09B0\u09C7\u09B0\u09C0, Erlang `:calendar` \u09AE\u09A1\u09BF\u0989\
  \u09B2\u09C7\u09B0 \u09B6\u0995\u09CD\u09A4\u09BF \u09B8\u09AE\u09CD\u09AE\u09BF\
  \u09B2\u09A8\u09C7, \u09A4\u09BE\u09B0\u09BF\u0996\u0997\u09C1\u09B2\u09BF \u09AA\
  \u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE\u09B0 \u09AA\u09CD\u09B0\u09BE\u09A5\
  \u09AE\u09BF\u0995 \u09B8\u09AE\u09B0\u09CD\u09A5\u09A8 \u09AA\u09CD\u09B0\u09A6\
  \u09BE\u09A8 \u0995\u09B0\u09C7\u0964 \u0986\u09B0\u0993 \u099C\u099F\u09BF\u09B2\
  \u2026"
lastmod: '2024-03-17T18:47:43.680543-06:00'
model: gpt-4-0125-preview
summary: "Elixir-\u098F\u09B0 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\
  \u09BE\u09B0\u09CD\u09A1 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09C0\
  , Erlang `:calendar` \u09AE\u09A1\u09BF\u0989\u09B2\u09C7\u09B0 \u09B6\u0995\u09CD\
  \u09A4\u09BF \u09B8\u09AE\u09CD\u09AE\u09BF\u09B2\u09A8\u09C7, \u09A4\u09BE\u09B0\
  \u09BF\u0996\u0997\u09C1\u09B2\u09BF \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\
  \u09BE\u09B0 \u09AA\u09CD\u09B0\u09BE\u09A5\u09AE\u09BF\u0995 \u09B8\u09AE\u09B0\
  \u09CD\u09A5\u09A8 \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964\
  \ \u0986\u09B0\u0993 \u099C\u099F\u09BF\u09B2 \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\
  \u099C\u09A8\u09C0\u09AF\u09BC\u09A4\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF, `Timex`\
  \ \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09C0 \u098F\u0995\u099F\u09BF\
  \ \u099C\u09A8\u09AA\u09CD\u09B0\u09BF\u09AF\u09BC \u09AA\u099B\u09A8\u09CD\u09A6\
  , \u09AF\u09BE \u09A4\u09BE\u09B0\u09BF\u0996, \u09B8\u09AE\u09AF\u09BC \u098F\u09AC\
  \u0982 \u099F\u09BE\u0987\u09AE\u099C\u09CB\u09A8\u0997\u09C1\u09B2\u09BF \u09A8\
  \u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE\u09B0 \u09AC\u09CD\
  \u09AF\u09BE\u09AA\u0995 \u09AC\u09C8\u09B6\u09BF\u09B7\u09CD\u099F\u09CD\u09AF\
  \ \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964\n\n#."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A4\
  \u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 30
---

## কীভাবে:
Elixir-এর স্ট্যান্ডার্ড লাইব্রেরী, Erlang `:calendar` মডিউলের শক্তি সম্মিলনে, তারিখগুলি পার্স করার প্রাথমিক সমর্থন প্রদান করে। আরও জটিল প্রয়োজনীয়তার জন্য, `Timex` লাইব্রেরী একটি জনপ্রিয় পছন্দ, যা তারিখ, সময় এবং টাইমজোনগুলি নিয়ে কাজ করার ব্যাপক বৈশিষ্ট্য প্রদান করে।

### Elixir-এর স্ট্যান্ডার্ড লাইব্রেরী ব্যবহার করে
```elixir
date_string = "2023-04-21"
{:ok, date} = Date.from_iso8601(date_string)
IO.inspect(date)  # => ~D[2023-04-21]
```

টাইমজোন তথ্যসহ একটি ডেটটাইম স্ট্রিং পার্স করতে চাইলে, আপনি সরাসরি Erlang ফাংশন ব্যবহার করতে পারেন, তবে মনে রাখবেন যে সরাসরি টাইমজোন হ্যান্ডলিং Elixir-এর স্ট্যান্ডার্ড Date মডিউলের একটি অংশ নয়।
```elixir
datetime_string = "2023-04-21T15:30:00Z"
{:ok, datetime, 0} = DateTime.from_iso8601(datetime_string)
IO.inspect(datetime)  # => #DateTime<2023-04-21 15:30:00Z>
```

### Timex ব্যবহার করে
প্রথমে, আপনার mix.exs ডিপেনডেন্সিতে `Timex` যোগ করুন:
```elixir
def deps do
  [
    {:timex, "~> 3.7"}
  ]
end
```
তারপর, নতুন নির্ভরতা আনার জন্য `mix deps.get` চালান।

এখানে আপনি কীভাবে Timex ব্যবহার করে একটি স্ট্রিং থেকে তারিখ পার্স করবেন, দেখানো হল:
```elixir
import Timex

date_string = "April 21, 2023"
date = Timex.parse!(date_string, "{Mfull} {D}, {YYYY}")
IO.inspect(date)  # => ~N[2023-04-21 00:00:00]
```

Timex বিভিন্ন ফরম্যাট পার্স করার সুযোগ দেয় এবং এমনকি প্রাকৃতিক ভাষা তারিখগুলিকেও সমর্থন করে, যা এটিকে ব্যবহারকারীর ইনপুট অথবা বাহ্যিক ডেটা উৎস থেকে তারিখ এবং সময় পার্স করার ক্ষেত্রে অত্যন্ত নমনীয় করে তোলে।

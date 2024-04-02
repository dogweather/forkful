---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:16:19.533516-06:00
description: "\u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982\u09DF\
  \u09C7 \u099F\u09C7\u0995\u09CD\u09B8\u099F \u0996\u09C1\u0981\u099C\u09C7 \u09AC\
  \u09C7\u09B0 \u0995\u09B0\u09BE \u098F\u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\
  \u09B8\u09CD\u09A5\u09BE\u09AA\u09A8 \u0995\u09B0\u09BE \u09AE\u09CC\u09B2\u09BF\
  \u0995 \u098F\u09AC\u0982 \u0985\u09AA\u09B0\u09BF\u09B9\u09BE\u09B0\u09CD\u09AF\
  ; \u09AE\u09C2\u09B2\u09A4 \u098F\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u0997\u09C1\u09B2\u09CB \u0996\u09C1\u0981\u099C\u09C7 \u09AC\u09C7\u09B0\
  \ \u0995\u09B0\u09BE \u098F\u09AC\u0982 \u09A4\u09BE\u09A6\u09C7\u09B0 \u09AC\u09A6\
  \u09B2\u09C7 \u09A6\u09C7\u0993\u09DF\u09BE\u09B0 \u0995\u09BE\u099C\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\u2026"
lastmod: '2024-03-17T18:47:43.651236-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982\u09DF\
  \u09C7 \u099F\u09C7\u0995\u09CD\u09B8\u099F \u0996\u09C1\u0981\u099C\u09C7 \u09AC\
  \u09C7\u09B0 \u0995\u09B0\u09BE \u098F\u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\
  \u09B8\u09CD\u09A5\u09BE\u09AA\u09A8 \u0995\u09B0\u09BE \u09AE\u09CC\u09B2\u09BF\
  \u0995 \u098F\u09AC\u0982 \u0985\u09AA\u09B0\u09BF\u09B9\u09BE\u09B0\u09CD\u09AF\
  ; \u09AE\u09C2\u09B2\u09A4 \u098F\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u0997\u09C1\u09B2\u09CB \u0996\u09C1\u0981\u099C\u09C7 \u09AC\u09C7\u09B0\
  \ \u0995\u09B0\u09BE \u098F\u09AC\u0982 \u09A4\u09BE\u09A6\u09C7\u09B0 \u09AC\u09A6\
  \u09B2\u09C7 \u09A6\u09C7\u0993\u09DF\u09BE\u09B0 \u0995\u09BE\u099C\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\u2026"
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\u09A7\
  \u09BE\u09A8 \u098F\u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\u09A5\
  \u09BE\u09AA\u09A8"
weight: 10
---

## কী এবং কেন?

প্রোগ্রামিংয়ে টেক্সট খুঁজে বের করা এবং প্রতিস্থাপন করা মৌলিক এবং অপরিহার্য; মূলত এটি স্ট্রিং গুলো খুঁজে বের করা এবং তাদের বদলে দেওয়ার কাজ। প্রোগ্রামাররা সমস্ত সময় কোডবেস আপডেট করা, টেক্সট ডাটা প্রসেস করা, অথবা শুধু সাধারণ সম্পাদনার কাজের জন্য এটি করে থাকে।

## কীভাবে:

এলিক্সিরে, আপনি `String` মডিউল ব্যবহার করে দ্রুত খুঁজে পেয়ে প্রতিস্থাপনের অপারেশন করতে পারেন। এখানে আপনি এটি যেভাবে করবেন:

```elixir
original_text = "I heart Elixir!"

# সাধারণ প্রতিস্থাপন
replaced_text = String.replace(original_text, "heart", "❤️")
IO.puts replaced_text  # আউটপুট: I ❤️ Elixir!

# গ্লোবাল প্রতিস্থাপন একটি প্যাটার্ন দিয়ে
replaced_text_global = String.replace(original_text, ~r/eart|Eli/, "❤️", global: true)
IO.puts replaced_text_global  # আউটপুট: I ❤️ ❤️xir!

# কেস অনুবেদনহীন প্রতিস্থাপন
insensitive_replace = String.replace(original_text, "ELIXIR", "❤️", global: true, case_insensitive: true)
IO.puts insensitive_replace  # আউটপুট: I heart ❤️!
```

## গভীরে যাও

টেক্সট খুঁজে পেয়ে প্রতিস্থাপনের ধারণা কম্পিউটিংয়ের শুরু থেকেই আছে; এটি একটি ওয়ার্ড ডকুমেন্টে 'ফাইন্ড এবং রিপ্লেস' এর মতো, কিন্তু কোডের জন্য। এলিক্সিরে, এটি মূলত প্যাটার্ন ম্যাচিং এবং স্ট্রিং নিয়ে কার্যকরীভাবে কাজ করার ধারণা।

`String.replace/4` ফাংশন এলিক্সিরের প্যাটার্ন ম্যাচিং সামর্থ্যকে কাজে লাগায়, যা আপনাকে শুধু স্থির স্ট্রিং নয় বরং রেগেক্স প্যাটার্ন গুলোর সাথেও ম্যাচ করার সুযোগ দেয়, যা গুরুত্বপূর্ণ লচ্ছনতার সাথে আসে। পর্দার অন্তরালে, এলিক্সির এরল্যাংয়ের শক্তিশালী স্ট্রিং হ্যান্ডলিং ব্যবহার করে, যা টেক্সট প্রসেসিং কাজে দৃঢ় এবং দক্ষ।

বিল্ট-ইন `String` মডিউলের বিকল্প হিসেবে আপনি আরও জটিল কেসের জন্য নিজ নিজ ফাংশন লিখতে পারেন অথবা বিভিন্নভাবে স্ট্রিং হ্যান্ডলিংকে জড়িত থার্ড-পার্টি লাইব্রেরিগুলো ব্যবহার করতে পারেন। তবে, বেশিরভাগ ব্যবহারের ক্ষেত্রে, বিল্ট-ইন ফাংশনগুলো অতিরিক্ত নির্ভরতা ছাড়াই কাজ সম্পন্ন করবে।

একটি পরিবর্তনশীল ভাষা হিসেবে, মনে রাখবেন প্রত্যেকটি প্রতিস্থাপন ফাংশন একটি নতুন স্ট্রিং ফিরিয়ে দেয় - মূল স্ট্রিং অপরিবর্তিত থাকে। এটি কিছু অন্য ভাষা থেকে আলাদা যেখানে আপনি হয়তো স্ট্রিংটি স্থানে পরিবর্তন করতে পারেন।

## দেখুন আরও

- এলিক্সিরের `String` মডিউল ডক্স: [https://hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)
- এলিক্সিরে রেগেক্স: [https://hexdocs.pm/elixir/Regex.html](https://hexdocs.pm/elixir/Regex.html)
- এলিক্সিরে প্যাটার্ন ম্যাচিং সম্পর্কে জানুন: [https://elixir-lang.org/getting-started/pattern-matching.html](https://elixir-lang.org/getting-started/pattern-matching.html)

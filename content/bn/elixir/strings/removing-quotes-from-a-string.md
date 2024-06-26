---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:12:44.846959-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Elixir-\u098F '\u0989\u09A6\u09CD\
  \u09A7\u09C3\u09A4\u09BF \u09B8\u09B0\u09BE\u09A8\u09CB' \u09AB\u09BE\u0982\u09B6\
  \u09A8\u099F\u09BF \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09A8\u09BF\u09B0\u09CD\u09AE\
  \u09BF\u09A4 \u09A8\u09C7\u0987, \u09A4\u09AC\u09C7 \u09AA\u09CD\u09AF\u09BE\u099F\
  \u09BE\u09B0\u09CD\u09A8 \u09AE\u09CD\u09AF\u09BE\u099A\u09BF\u0982 \u0985\u09A5\
  \u09AC\u09BE `String` \u09AB\u09BE\u0982\u09B6\u09A8\u0997\u09C1\u09B2\u09BF\u09B0\
  \ \u09B8\u09BE\u09B9\u09BE\u09AF\u09CD\u09AF\u09C7 \u09A8\u09BF\u099C\u09C7\u09B0\
  \ \u098F\u0995\u099F\u09BF \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE \u0996\u09C1\
  \u09AC\u0987 \u09B8\u09B9\u099C\u0964 \u098F\u0987\u2026"
lastmod: '2024-03-17T18:47:43.654857-06:00'
model: gpt-4-0125-preview
summary: "Elixir-\u098F '\u0989\u09A6\u09CD\u09A7\u09C3\u09A4\u09BF \u09B8\u09B0\u09BE\
  \u09A8\u09CB' \u09AB\u09BE\u0982\u09B6\u09A8\u099F\u09BF \u0985\u09A8\u09CD\u09A4\
  \u09B0\u09CD\u09A8\u09BF\u09B0\u09CD\u09AE\u09BF\u09A4 \u09A8\u09C7\u0987, \u09A4\
  \u09AC\u09C7 \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8 \u09AE\u09CD\
  \u09AF\u09BE\u099A\u09BF\u0982 \u0985\u09A5\u09AC\u09BE `String` \u09AB\u09BE\u0982\
  \u09B6\u09A8\u0997\u09C1\u09B2\u09BF\u09B0 \u09B8\u09BE\u09B9\u09BE\u09AF\u09CD\u09AF\
  \u09C7 \u09A8\u09BF\u099C\u09C7\u09B0 \u098F\u0995\u099F\u09BF \u09A4\u09C8\u09B0\
  \u09BF \u0995\u09B0\u09BE \u0996\u09C1\u09AC\u0987 \u09B8\u09B9\u099C\u0964 \u098F\
  \u0987 \u09B8\u09CD\u09A8\u09BF\u09AA\u09C7\u099F\u0997\u09C1\u09B2\u09BF \u09A6\
  \u09C7\u0996\u09C1\u09A8."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u0989\
  \u09A6\u09CD\u09A7\u09C3\u09A4\u09BF \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\
  \u09BE"
weight: 9
---

## কিভাবে:
Elixir-এ 'উদ্ধৃতি সরানো' ফাংশনটি অন্তর্নির্মিত নেই, তবে প্যাটার্ন ম্যাচিং অথবা `String` ফাংশনগুলির সাহায্যে নিজের একটি তৈরি করা খুবই সহজ। এই স্নিপেটগুলি দেখুন:

```elixir
# প্যাটার্ন ম্যাচিং ব্যবহার করে
def unquote_string("\"" <> quoted_string <> "\""), do: quoted_string
def unquote_string("'" <> quoted_string <> "'"), do: quoted_string
def unquote_string(quoted_string), do: quoted_string

# নমুনা ব্যবহার
unquote_string("\"Hello, World!\"") # => "Hello, World!"
unquote_string("'Hello, World!'")   # => "Hello, World!"

# String.trim/1 ব্যবহার করে
def unquote_string(string), do: String.trim(string, "'\"")

# নমুনা ব্যবহার
unquote_string("\"Hello, World!\"") # => "Hello, World!"
unquote_string("'Hello, World!'")   # => "Hello, World!"
```

উভয় পদ্ধতির জন্য আউটপুট হবে:
```
"Hello, World!"
```

## গভীর ডুব
আগেকার দিনে, স্ট্রিং মধ্যে উদ্ধৃতি চিহ্ন গুলি ছিল এক রকমের মাইনফিল্ড—ভুলভাবে হ্যান্ডল করলে, বুম, সিনট্যাক্স ত্রুটি অথবা নিরাপত্তা ফাঁকসমূহ সৃষ্টি হত। Elixir-এ, প্যাটার্ন ম্যাচিং আপনার স্ট্রিংগুলিকে লেগো ব্লকের মত করে ত্রুটিমুক্ত করে, যা আপনাকে অংশ বিশেষ আলাদা করে নেওয়া এবং সঠিকভাবে পুনর্নির্মাণ করতে দেয়। এর শক্তিশালী `String` মডিউলটি `trim` ফাংশনগুলির মাধ্যমে নমনীয়ভাবে উদ্ধৃতি চিহ্নগুলি নির্মূল করে। বিকল্পগুলি? নিয়মিত এক্সপ্রেশনগুলি উদ্ধৃতি চিহ্নগুলিকে দূরে সরিয়ে দিতে পারে এবং বাহ্যিক লাইব্রেরিগুলি মৌলিক স্ট্রিপিং-এর চেয়ে বেশি কিছু প্রয়োজন হলে অতিরিক্ত ক্ষমতা সরবরাহ করতে পারে।

## আরও দেখুন
এগুলির সাথে আরও গভীরে ডুব দিন:
- [Elixir's String মডিউল](https://hexdocs.pm/elixir/String.html)
- [Elixir-এ প্যাটার্ন ম্যাচিং সম্পর্কে আরও জানুন](https://elixir-lang.org/getting-started/pattern-matching.html)
- [Elixir-এ নিয়মিত এক্সপ্রেশন (Regex মডিউল)](https://hexdocs.pm/elixir/Regex.html)

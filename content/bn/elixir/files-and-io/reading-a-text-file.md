---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:08:28.416417-06:00
description: "\u098F\u0995\u099F\u09BE \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\
  \u09BE\u0987\u09B2 \u09AA\u09A1\u09BC\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2\
  \u09CB \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09C7 \u09AB\u09BE\
  \u0987\u09B2 \u09A5\u09C7\u0995\u09C7 \u09A1\u09BE\u099F\u09BE \u099F\u09C7\u09A8\
  \u09C7 \u0986\u09A8\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\
  \u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BE \u0995\u09A8\u099F\u09C7\u09A8\
  \u09CD\u099F \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE \u09AC\
  \u09BE \u09AC\u09BF\u09B6\u09CD\u09B2\u09C7\u09B7\u09A3 \u0995\u09B0\u09BE\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u09A8, \u09AF\
  \u09C7\u09AE\u09A8 \u0995\u09A8\u09AB\u09BF\u0997 \u09AA\u09A1\u09BC\u09BE,\u2026"
lastmod: '2024-03-17T18:47:43.689227-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BE \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\
  \u0987\u09B2 \u09AA\u09A1\u09BC\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2\u09CB\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09C7 \u09AB\u09BE\u0987\
  \u09B2 \u09A5\u09C7\u0995\u09C7 \u09A1\u09BE\u099F\u09BE \u099F\u09C7\u09A8\u09C7\
  \ \u0986\u09A8\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BE \u0995\u09A8\u099F\u09C7\u09A8\u09CD\
  \u099F \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE \u09AC\u09BE\
  \ \u09AC\u09BF\u09B6\u09CD\u09B2\u09C7\u09B7\u09A3 \u0995\u09B0\u09BE\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u09A8, \u09AF\u09C7\
  \u09AE\u09A8 \u0995\u09A8\u09AB\u09BF\u0997 \u09AA\u09A1\u09BC\u09BE,\u2026"
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\
  \u09BC\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

একটা টেক্সট ফাইল পড়া মানে হলো প্রোগ্রামে ফাইল থেকে ডাটা টেনে আনা। প্রোগ্রামাররা এটা কনটেন্ট প্রক্রিয়া বা বিশ্লেষণ করার জন্য করে থাকেন, যেমন কনফিগ পড়া, লগ বিশ্লেষণ করা, অথবা ডাটা ইম্পোর্ট করা।

## কিভাবে:

এখানে `example.txt` নামে একটা টেক্সট ফাইলের সম্পূর্ণ কনটেন্ট পড়ার পদ্ধতি দেওয়া হল:

```elixir
File.read("example.txt")
```

যদি `example.txt` এ "Hello, Elixir!" থাকে তবে নমুনা আউটপুট:

```elixir
{:ok, "Hello, Elixir!"}
```

ফাইলটিকে প্রতি লাইন পড়ার জন্য:

```elixir
File.stream!("example.txt")
|> Enum.each(fn line -> IO.puts(line) end)
```

এটা `example.txt` এর প্রতিটি লাইন কনসোলে প্রিন্ট করবে।

## বিস্তারিত আলোচনা 

এলিক্সিরে, `File.read/1` ও `File.stream!/1` হলো টেক্সট ফাইল পড়ার সাধারণ পদ্ধতি। ঐতিহাসিকভাবে, প্রোগ্রামিংয়ে ফাইল পড়ার প্রয়োজন ডাটা স্টোর করে রাখা ও পুনঃপ্রাপ্তি থেকে উৎপন্ন। প্রাথমিক কম্পিউটিং যুগে, এটি পাঞ্চ কার্ড অথবা ম্যাগনেটিক টেপের মাধ্যমে করা হত। আজকে, আমরা SSDs, HDDs, এবং আরও অনেক ধরনের স্টোরেজ ডিভাইস ব্যবহার করি।

`File.read/1` এর একটি বিকল্প হলো `File.read!/1`, যা কোন সমস্যা হলে একটি ত্রুটি উত্থাপন করে একটি টিউপল ফেরত দেওয়ার পরিবর্তে। একইভাবে, `File.stream!/1` ব্যর্থতা উপর একটি ত্রুটি উত্থাপন করে `File.stream/1` থেকে আলাদা।

অভ্যন্তরীণভাবে, এই বাস্তবায়নটি বাইনারি ডাটা নিয়ে সামলায়। এলিক্সির টেক্সটকে বাইনারিতে রূপান্তর করে, যা অধীনের বাইট এবং এনকোডিং নিয়ন্ত্রণ করে।

## আরও দেখুন:

- এলিক্সিরের অফিসিয়াল `File` মডিউল ডকিউমেন্টেশন: [https://hexdocs.pm/elixir/File.html](https://hexdocs.pm/elixir/File.html)

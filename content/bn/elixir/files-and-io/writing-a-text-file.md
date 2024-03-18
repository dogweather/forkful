---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:39:44.842387-06:00
description: "\u0987\u09B2\u09BF\u0995\u09CD\u09B8\u09BF\u09B0\u09C7 \u099F\u09C7\u0995\
  \u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2\u09C7 \u09B2\u09C7\u0996\u09BE \u09A1\
  \u09C7\u09AD\u09C7\u09B2\u09AA\u09BE\u09B0\u09A6\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u098F\u0995\u099F\u09BF \u0985\u09AA\u09B0\u09BF\u09B9\u09BE\u09B0\u09CD\u09AF\
  \ \u09A6\u0995\u09CD\u09B7\u09A4\u09BE, \u09A1\u09BE\u099F\u09BE \u09B8\u0982\u09B0\
  \u0995\u09CD\u09B7\u09A3, \u09B2\u0997\u09BF\u0982 \u09AC\u09BE \u09AE\u09BE\u09A8\
  \u09AC-\u09AA\u09BE\u09A0\u09CD\u09AF\u09AF\u09CB\u0997\u09CD\u09AF \u0995\u09A8\
  \u09CD\u099F\u09C7\u09A8\u09CD\u099F \u09B0\u09AA\u09CD\u09A4\u09BE\u09A8\u09BF\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u0985\u09A4\u09CD\u09AF\u09BE\u09AC\u09B6\u09CD\u09AF\
  \u0995\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\
  \u09BE\u2026"
lastmod: '2024-03-17T18:47:43.690438-06:00'
model: gpt-4-0125-preview
summary: "\u0987\u09B2\u09BF\u0995\u09CD\u09B8\u09BF\u09B0\u09C7 \u099F\u09C7\u0995\
  \u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2\u09C7 \u09B2\u09C7\u0996\u09BE \u09A1\
  \u09C7\u09AD\u09C7\u09B2\u09AA\u09BE\u09B0\u09A6\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u098F\u0995\u099F\u09BF \u0985\u09AA\u09B0\u09BF\u09B9\u09BE\u09B0\u09CD\u09AF\
  \ \u09A6\u0995\u09CD\u09B7\u09A4\u09BE, \u09A1\u09BE\u099F\u09BE \u09B8\u0982\u09B0\
  \u0995\u09CD\u09B7\u09A3, \u09B2\u0997\u09BF\u0982 \u09AC\u09BE \u09AE\u09BE\u09A8\
  \u09AC-\u09AA\u09BE\u09A0\u09CD\u09AF\u09AF\u09CB\u0997\u09CD\u09AF \u0995\u09A8\
  \u09CD\u099F\u09C7\u09A8\u09CD\u099F \u09B0\u09AA\u09CD\u09A4\u09BE\u09A8\u09BF\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u0985\u09A4\u09CD\u09AF\u09BE\u09AC\u09B6\u09CD\u09AF\
  \u0995\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\
  \u09BE\u2026"
title: "\u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\
  \u0987\u09B2 \u09B2\u09BF\u0996\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

ইলিক্সিরে টেক্সট ফাইলে লেখা ডেভেলপারদের জন্য একটি অপরিহার্য দক্ষতা, ডাটা সংরক্ষণ, লগিং বা মানব-পাঠ্যযোগ্য কন্টেন্ট রপ্তানির জন্য অত্যাবশ্যক। প্রোগ্রামাররা অ্যাপ্লিকেশনের অবস্থা, ডিবাগ তথ্য, কনফিগারেশন, বা যেকোনো ডাটা এক্সচেঞ্জ সংরক্ষণের জন্য এই কাজ করে যেগুলো টেক্সটের মতো সর্বজনীন ফরম্যাট পছন্দ করে।

## কিভাবে:

ইলিক্সির অন্তর্নির্মিত মডিউলগুলির সাথে ফাইল হ্যান্ডলিং সরল করে তোলে। ফাইলে লেখার প্রাথমিক উপায় হল `File.write/2` অথবা `File.write!/2` ফাংশনের ব্যবহার, যেখানে পূর্ববর্তীটি একটি `:ok` অথবা `:error` টুপল রিটার্ন করে এবং পরবর্তীটি ব্যর্থতায় একটি ত্রুটি উত্থাপন করে।

এখানে একটি সহজ উদাহরণ দেওয়া হোল:

```elixir
# একটি ফাইলে লেখা, সাধারণ বার্তা
File.write("hello.txt", "Hello, World!")

# যখন আপনি কোডটি চালান, এটি 'hello.txt' নামের একটি ফাইল তৈরি করে, "Hello, World!" কন্টেন্ট সহ
```

ফাইলগুলিতে যোগ করার জন্য, আপনি `File.open/3` ব্যবহার করবেন `[:write, :append]` অপশনগুলির সাথে, তারপর `IO.binwrite/2` ব্যবহার করে কন্টেন্ট যোগ করবেন:

```elixir
# একটি ফাইলে যোগ করা
{:ok, file} = File.open("hello.txt", [:write, :append])
IO.binwrite(file, "\nLet's add another line.")
File.close(file)

# এখন 'hello.txt' তে দ্বিতীয় লাইন "Let's add another line." যুক্ত হয়
```

যদি আপনি বড় ডাটা নিয়ে কাজ করছেন বা লেখার প্রক্রিয়া নিয়ন্ত্রণে আরও বেশি কিছুর প্রয়োজন হয়, আপনি ফাইলে ডাটাকে অলসিতা সহকারে লিখতে `Stream` মডিউল ব্যবহার করতে পারেন:

```elixir
# বড় ডাটাসেটে অলসিতাসহ লেখা
stream_data = Stream.iterate(0, &(&1 + 1))
            |> Stream.map(&("Number: #{&1}\n"))
            |> Stream.take(10)

File.open!("numbers.txt", [:write], fn file ->
  Enum.each(stream_data, fn line ->
    IO.write(file, line)
  end)
end)

# এটি 'numbers.txt' নামে একটি ফাইল তৈরি করে, প্রতিটি নম্বর 0 থেকে 9 প্রতিটি একটি নতুন লাইনে লেখা হয়।
```

যে সব প্রকল্পে আরও উন্নত ফাইল হ্যান্ডলিং প্রয়োজন, আপনি `CSV` এর মতো তৃতীয়-পক্ষীয় লাইব্রেরিগুলির দিকে তাকাতে পারেন, যা CSV ফাইল ম্যানিপুলেশনের জন্য বিশেষায়িত ফাংশনালিটিগুলি অফার করে তবে মনে রাখবেন, অনেক ক্ষেত্রে, ইলিক্সিরের অন্তর্নির্মিত সামগ্রীগুলি যথেষ্ট।

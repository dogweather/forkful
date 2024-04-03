---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:17.509786-06:00
description: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u0995\
  \u09CD\u09B8\u099F\u09CD\u09B0\u09BE\u0995\u09CD\u099F \u0995\u09B0\u09BE \u09AE\
  \u09BE\u09A8\u09C7 \u09B9\u09B2 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\
  \u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\
  \u09B7\u09CD\u099F \u0985\u0982\u09B6\u0997\u09C1\u09B2\u09BF \u09AC\u09C7\u09B0\
  \ \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F\
  \ \u09A1\u09C7\u099F\u09BE \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\u09AA\u09C1\u09B2\
  \u09C7\u099F \u098F\u09AC\u0982 \u09AC\u09BF\u09B6\u09CD\u09B2\u09C7\u09B7\u09A3\
  \ \u0995\u09B0\u09A4\u09C7, \u0985\u09A5\u09AC\u09BE\u2026"
lastmod: '2024-03-17T18:47:43.656083-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u0995\
  \u09CD\u09B8\u099F\u09CD\u09B0\u09BE\u0995\u09CD\u099F \u0995\u09B0\u09BE \u09AE\
  \u09BE\u09A8\u09C7 \u09B9\u09B2 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\
  \u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\
  \u09B7\u09CD\u099F \u0985\u0982\u09B6\u0997\u09C1\u09B2\u09BF \u09AC\u09C7\u09B0\
  \ \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F\
  \ \u09A1\u09C7\u099F\u09BE \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\u09AA\u09C1\u09B2\
  \u09C7\u099F \u098F\u09AC\u0982 \u09AC\u09BF\u09B6\u09CD\u09B2\u09C7\u09B7\u09A3\
  \ \u0995\u09B0\u09A4\u09C7, \u0985\u09A5\u09AC\u09BE \u0995\u09C7\u09AC\u09B2\u09AE\
  \u09BE\u09A4\u09CD\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\
  \u09C0\u09A6\u09C7\u09B0 \u0995\u09BE\u099B\u09C7 \u09AA\u09CD\u09B0\u09BE\u09B8\
  \u0999\u09CD\u0997\u09BF\u0995 \u09A4\u09A5\u09CD\u09AF\u09C7\u09B0 \u0985\u0982\
  \u09B6\u0997\u09C1\u09B2\u09BF \u09AA\u09CD\u09B0\u09A6\u09B0\u09CD\u09B6\u09A8\
  \ \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u0995\u09B0\u09C7 \u09A5\u09BE\
  \u0995\u09C7\u0964."
title: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AC\u09C7\u09B0\
  \ \u0995\u09B0\u09BE"
weight: 6
---

## কিভাবে:
এলিক্সিরে, আপনি `String` মডিউল ব্যবহার করে সাবস্ট্রিং এক্সট্রাক্ট করতে পারেন। এই রকম:

```elixir
str = "Hello, World!"

# রেঞ্জ দ্বারা একটি সাবস্ট্রিং এক্সট্রাক্ট করা
substr = String.slice(str, 7, 5)
IO.puts(substr)  # => World

# স্ট্রিংয়ের শেষ পর্যন্ত একটি সাবস্ট্রিং এক্সট্রাক্ট করা
substr_end = String.slice(str, 7)
IO.puts(substr_end)  # => World!

# একটি একক অক্ষর পাওয়া (টেকনিকালি এটিও একটি সাবস্ট্রিং)
char = String.at(str, 1)
IO.puts(char)  # => e
```

এই স্নিপেটগুলি দেখায় স্ট্রিংকে ইনডেক্স রেঞ্জ দ্বারা, স্ট্রিংয়ের শেষ পর্যন্ত, এবং একটি একক অক্ষর গ্রহণ করা।

## গভীর ডুব দেওয়া
এলিক্সিরের স্ট্রিং সম্পর্কিত ধারণা এটির এরলাং ঐতিহ্য দ্বারা প্রভাবিত, যা স্ট্রিং সঞ্চয়ের জন্য বাইনারিজ ব্যবহার করে। নাল-টার্মিনেটেড স্ট্রিং ব্যবহার করে যেমন ভাষাগুলিতে এক্সট্রাকশন হয়, এলিক্সির তার থেকে ভিন্ন। এলিক্সিরের সাবস্ট্রিংগুলি ইউটিএফ-8 এবং বাইনারি-নিরাপদ, অর্থাৎ এগুলি অক্ষরের সীমাবদ্ধতা মেনে চলে।

অতীতে, বিভিন্ন প্রোগ্রামিং ভাষা এবং সিস্টেমগুলিতে স্ট্রিং হ্যান্ডেল করার নিজস্ব উপায় ছিল, যা প্রায়ই আন্তর্জাতিকীকরণ এবং মেমরি ম্যানেজমেন্টে সমস্যা তৈরি করত। এলিক্সিরের বাইনারি-ভিত্তিক স্ট্রিংগুলি একটি বিশ্বব্যাপী এবং দক্ষ পদ্ধতি প্রদান করে স্ট্রিং ম্যানিপুলেশনের জন্য।

এলিক্সিরে `String.slice` এবং `String.at` ছাড়াও সাবস্ট্রিং এক্সট্রাক্ট করার বিকল্পগুলি মূলত রেগেক্স অপারেশনস অথবা স্ট্রিং প্যাটার্ন ম্যাচিং জড়িত, যা উভয়ই শক্তিশালী হতে পারে কিন্তু আরও জটিল হতে পারে।

বাস্তবায়নের বিস্তারিত ব্যাপার অত্যন্ত জরুরি কারণ সাবস্ট্রিং এক্সট্রাকশন হতে পারে সম্পদ ব্যবহারের দিক থেকে ইনটেনসিভ, বিশেষ করে যখন বড় স্ট্রিংগুলির ভুলভাবে হ্যান্ডলিং অথবা বহু অপারেশন পরিচালনা করা হয়। এলিক্সিরের ফাংশনাল প্রকৃতি স্ট্রিংগুলি প্রক্রিয়া করার একটি উপায় উৎসাহিত করে, যা প্যাটার্ন ম্যাচিং এবং রিকার্শনের সুবিধা নিয়ে কাজ করে, যা পারফর্মেন্স এবং কোডের পরিষ্কারতায় সাহায্য করতে পারে।

## আরও দেখুন
আরও পড়াশোনা এবং বিস্তারিত ডকুমেন্টেশনের জন্য, আপনি এই লিংকগুলি দেখতে পারেন:

- এলিক্সিরের অফিসিয়াল `String` মডিউল ডকুমেন্টেশন: [hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)
- এলিক্সিরে বাইনারি এবং স্ট্রিংগুলি বুঝতে: [elixir-lang.org/getting-started/binaries-strings-and-char-lists.html](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html)
- এলিক্সির স্কুলের স্ট্রিং এবং প্যাটার্ন ম্যাচিং সম্পর্কে: [elixirschool.com/en/lessons/basics/strings](https://elixirschool.com/en/lessons/basics/strings/) এবং [elixirschool.com/en/lessons/basics/pattern-matching](https://elixirschool.com/en/lessons/basics/pattern-matching/)
- এলিক্সিরে রেগুলার এক্সপ্রেশন: [hexdocs.pm/elixir/Regex.html](https://hexdocs.pm/elixir/Regex.html)

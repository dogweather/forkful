---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:24:16.052338-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: IEx \u099A\u09BE\u09B2\u09C1 \u0995\
  \u09B0\u09A4\u09C7, \u0986\u09AA\u09A8\u09BE\u09B0 \u099F\u09BE\u09B0\u09CD\u09AE\
  \u09BF\u09A8\u09BE\u09B2 \u0996\u09C1\u09B2\u09C1\u09A8 \u098F\u09AC\u0982 \u099F\
  \u09BE\u0987\u09AA \u0995\u09B0\u09C1\u09A8 `iex`\u0964 \u098F\u0996\u09BE\u09A8\
  \u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u09AC\u09BE\u09A6."
lastmod: '2024-03-17T18:47:43.671234-06:00'
model: gpt-4-0125-preview
summary: "IEx \u099A\u09BE\u09B2\u09C1 \u0995\u09B0\u09A4\u09C7, \u0986\u09AA\u09A8\
  \u09BE\u09B0 \u099F\u09BE\u09B0\u09CD\u09AE\u09BF\u09A8\u09BE\u09B2 \u0996\u09C1\
  \u09B2\u09C1\u09A8 \u098F\u09AC\u0982 \u099F\u09BE\u0987\u09AA \u0995\u09B0\u09C1\
  \u09A8 `iex`\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09B8\
  \u09CD\u09AC\u09BE\u09A6."
title: "\u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AF\u09BC\u09BE\u0995\u09CD\u099F\u09BF\
  \u09AD \u09B6\u09C7\u09B2 (REPL) \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09BE"
weight: 34
---

## কিভাবে:
IEx চালু করতে, আপনার টার্মিনাল খুলুন এবং টাইপ করুন `iex`। এখানে একটি স্বাদ:

```Elixir
iex> name = "Elixir Programmer"
"Elixir Programmer"
iex> String.length(name)
17
iex> Enum.map([1, 2, 3], fn num -> num * 3 end)
[3, 6, 9]
```

আউটপুটে পরিবর্তনশীল অ্যাসাইনমেন্ট, ফাংশন ফলাফল, এবং একটি অনামী ফাংশনের কাজ দেখানো উচিত।

## গভীর ডুব
IEx শেল এলিক্সিরের প্রারম্ভিক দিন থেকেই এর এক অংশ। এলিক্সিরের স্রষ্টা জোসে ভালিম, পাইথনের `python` এবং রুবির `irb` এর মতো অন্যান্য ভাষার ইন্টারেক্টিভ শেল থেকে অনুপ্রেরণা নিয়েছিলেন। যদিও IEx এই গুলির সাথে অনেক বৈশিষ্ট্য ভাগ করে, এটি এলিক্সিরের সমাকালীন প্রকৃতি সামলানো এবং অপূর্ণরূপে এরলাং VM ক্ষমতার সঙ্গে সম্পূর্ণ একীভূত করা হয়েছে।

এরলাং ইকোসিস্টেমে IEx এর বিকল্পগুলির মধ্যে `erl`, এরলাং শেল অন্তর্ভুক্ত। কিন্তু IEx একটি বেশি এলিক্সির-বান্ধব পরিবেশ প্রদান করে, যেমন ব্যাপক ট্যাব সম্পূর্ণতা, ইতিহাস, এবং সহায়িকাগুলির মতো বৈশিষ্ট্য সহ।

IEx REPL শুধুমাত্র একটি প্লেগ্রাউন্ড নয়; এটি একটি চলমান সিস্টেমে নির্বিঘ্নে সংযুক্ত হতে পারে। এটি লাইভ অ্যাপ্লিকেশনগুলি ডিবাগ করার জন্য অত্যন্ত গুরুত্বপূর্ণ। অন্তর্নিহিত বাস্তবায়ন BEAM (এরলাং VM) এ ভিত্তি করে এবং এটি শেলের মধ্যে হট কোড সোয়াপিং এর মতো বৈশিষ্ট্য সমর্থন করে।

## আরো দেখুন
আরও পড়ার জন্য এবং সম্পদের জন্য এগুলি দেখুন:

- [এলিক্সিরের IEx ডকুমেন্টেশন](https://hexdocs.pm/iex/IEx.html)
- [Interactive Elixir (IEx) - দ্য এলিক্সির শেল](https://elixir-lang.org/getting-started/introduction.html#interactive-elixir)
- [এরলাংয়ের `erl` ডকুমেন্টেশন](http://erlang.org/doc/man/erl.html)
- [এলিক্সিরের ইন্টারেক্টিভ শেল শেখা](https://elixirschool.com/en/lessons/basics/iex_helpers/)

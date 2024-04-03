---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:22.923611-06:00
description: "\u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u0985\u0995\u09CD\
  \u09B7\u09B0\u09C7\u09B0 \u0985\u09A8\u09C1\u0995\u09CD\u09B0\u09AE \u0996\u09C1\
  \u0981\u099C\u09C7 \u09AC\u09C7\u09B0 \u0995\u09B0\u09C7 \u09A4\u09BE \u09AE\u09C1\
  \u099B\u09C7 \u09AB\u09C7\u09B2\u09BE\u0987 \u09B9\u09B2 \u0985\u0995\u09CD\u09B7\
  \u09B0\u09C7\u09B0 \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8 \u09AE\
  \u09BF\u09B2\u09C7 \u09AF\u09BE\u0993\u09AF\u09BC\u09BE\u09B0 \u09B8\u09BE\u09A5\
  \u09C7 \u09B8\u09BE\u09A5\u09C7 \u09A4\u09BE \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\
  \u09B2\u09BE\u09B0 \u09AE\u09C2\u09B2 \u0989\u09A6\u09CD\u09A6\u09C7\u09B6\u09CD\
  \u09AF\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\
  \u09BE \u098F\u099F\u09BF \u09A1\u09C7\u099F\u09BE\u2026"
lastmod: '2024-03-17T18:47:43.650178-06:00'
model: gpt-4-0125-preview
summary: "\u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u0985\u0995\u09CD\
  \u09B7\u09B0\u09C7\u09B0 \u0985\u09A8\u09C1\u0995\u09CD\u09B0\u09AE \u0996\u09C1\
  \u0981\u099C\u09C7 \u09AC\u09C7\u09B0 \u0995\u09B0\u09C7 \u09A4\u09BE \u09AE\u09C1\
  \u099B\u09C7 \u09AB\u09C7\u09B2\u09BE\u0987 \u09B9\u09B2 \u0985\u0995\u09CD\u09B7\
  \u09B0\u09C7\u09B0 \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8 \u09AE\
  \u09BF\u09B2\u09C7 \u09AF\u09BE\u0993\u09AF\u09BC\u09BE\u09B0 \u09B8\u09BE\u09A5\
  \u09C7 \u09B8\u09BE\u09A5\u09C7 \u09A4\u09BE \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\
  \u09B2\u09BE\u09B0 \u09AE\u09C2\u09B2 \u0989\u09A6\u09CD\u09A6\u09C7\u09B6\u09CD\
  \u09AF\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\
  \u09BE \u098F\u099F\u09BF \u09A1\u09C7\u099F\u09BE \u09B8\u09CD\u09AF\u09BE\u09A8\
  \u09BF\u099F\u09BE\u0987\u099C\u09C7\u09B6\u09A8, \u0995\u09A8\u09CD\u099F\u09C7\
  \u09A8\u09CD\u099F \u09AB\u09B0\u09CD\u09AE\u09CD\u09AF\u09BE\u099F\u09BF\u0982\
  , \u0985\u09A5\u09AC\u09BE \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A8\u09BF\
  \u09AF\u09BC\u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09AA\
  \u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u0985\u09A8\u09C1\u09B8\u09BE\u09B0\u09C7\
  \ \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\u09AA\u09C1\u09B2\u09C7\u09B6\u09A8 \u0995\
  \u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u0995\u09B0\u09C7\u0964."
title: "\u098F\u0995\u099F\u09BF \u09A8\u09AE\u09C1\u09A8\u09BE \u09AE\u09C7\u09B2\
  \u09C7 \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09AE\u09C1\u099B\
  \u09C7 \u09AB\u09C7\u09B2\u09BE"
weight: 5
---

## কিভাবে:
Elixir এ, প্যাটার্ন ম্যাচিং অক্ষর মুছে ফেলতে `String.replace/4` ফাংশনের ব্যবহার করুন। নিচের নমুনাগুলি দেখুন:

```elixir
# একটি স্ট্রিং থেকে সংখ্যা মুছে ফেলুন
original_string = "Elixir2023Rocks!"
clean_string = String.replace(original_string, ~r/\d/, "")
IO.puts(clean_string) # আউটপুট: "ElixirRocks!"

# বিরামচিহ্ন সরান
punctuationless_string = String.replace(original_string, ~r/[[:punct:]]/, "")
IO.puts(punctuationless_string) # আউটপুট: "Elixir2023Rocks"

# স্পেস মুছে ফেলুন
no_whitespace_string = String.replace(original_string, ~r/\s/, "")
IO.puts(no_whitespace_string) # আউটপুট: "Elixir2023Rocks!"
```

## গভীর ডুব
স্ট্রিংয়ের মধ্যে অক্ষর মুছে ফেলার জন্য প্যাটার্ন ম্যাচিং এর ব্যবহার শুধু Elixir এ নয়, প্রায় সব প্রোগ্রামিং ভাষায় প্রচলিত, যা early Unix টুলগুলি যেমন `sed` এবং `grep` এর রেগুলার এক্সপ্রেশন (regex) দক্ষতার থেকে বিকশিত হয়েছে। `String.replace/4` এর বিকল্প পদ্ধতি হল প্যাটার্ন ম্যাচিং এবং পুনরাবৃত্তি ব্যবহার করে ম্যানুয়ালি একটি স্ট্রিং ট্রাভার্স করা এবং তা পরিবর্তন করা, কিন্তু এই পদ্ধতিটি সাধারণত বেশি বর্ণনাত্মক এবং জটিল হয়, যার ফলে বিল্ট-ইন regex ফাংশনগুলি হয়ে ওঠে অগ্রাধিকার। অন্তর্নিহিতভাবে, `String.replace/4` এলিক্সিরের এরল্যাঙ্গ ঐতিহ্যের সুবিধা নেয়, বিইএম ভার্চুয়াল মেশিনের শক্তিশালী প্যাটার্ন ম্যাচিং এবং স্ট্রিং ম্যানিপুলেশন দক্ষতা ব্যবহার করে।

## আরও দেখুন:
- Elixir `String` মডিউল ডকুমেন্টেশন: [https://hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)
- এলিক্সিরে Regex: [https://hexdocs.pm/elixir/Regex.html](https://hexdocs.pm/elixir/Regex.html)
- 'রেগুলার এক্সপ্রেশন শেখা': [https://www.regular-expressions.info/tutorial.html](https://www.regular-expressions.info/tutorial.html)
- এলিক্সির স্কুলের স্ট্রিং এবং প্যাটার্ন ম্যাচিং সম্পর্কিত মতামত: [https://elixirschool.com/en/lessons/basics/strings/](https://elixirschool.com/en/lessons/basics/strings/)

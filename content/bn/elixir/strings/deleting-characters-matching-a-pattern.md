---
title:                "একটি নমুনা মেলে অক্ষরগুলি মুছে ফেলা"
date:                  2024-03-17T17:47:22.923611-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

নির্দিষ্ট অক্ষরের অনুক্রম খুঁজে বের করে তা মুছে ফেলাই হল অক্ষরের প্যাটার্ন মিলে যাওয়ার সাথে সাথে তা মুছে ফেলার মূল উদ্দেশ্য। প্রোগ্রামাররা এটি ডেটা স্যানিটাইজেশন, কন্টেন্ট ফর্ম্যাটিং, অথবা স্ট্রিং নিয়ে নির্দিষ্ট প্রয়োজন অনুসারে ম্যানিপুলেশন করার জন্য করে।

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

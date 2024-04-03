---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:35.564447-06:00
description: "\u0995\u09C0\u09AD\u09BE\u09AC\u09C7: \u098F\u09B2\u09BF\u0995\u09CD\
  \u09B8\u09BF\u09B0 \u09A4\u09C3\u09A4\u09C0\u09AF\u09BC \u09AA\u0995\u09CD\u09B7\
  \u09C7\u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u0997\u09C1\u09B2\
  \u09BF\u09B0 \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u099B\u09BE\u09A1\
  \u09BC\u09BE\u0987 \u098F\u09B0 \u0985\u09AD\u09CD\u09AF\u09A8\u09CD\u09A4\u09B0\
  \u09C0\u09A3 \u09AB\u09BE\u0982\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0997\u09C1\
  \u09B2\u09BF\u0995\u09C7 \u0995\u09CD\u09AF\u09BE\u09AA\u09BF\u099F\u09BE\u09B2\u09BE\
  \u0987\u099C \u0995\u09B0\u09BE\u09B0 \u098F\u0995\u099F\u09BE \u09B8\u09CB\u099C\
  \u09BE \u0989\u09AA\u09BE\u09AF\u09BC \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\
  \u09B0\u09C7\u0964\u2026"
lastmod: '2024-03-17T18:47:43.649094-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u09B2\u09BF\u0995\u09CD\u09B8\u09BF\u09B0 \u09A4\u09C3\u09A4\u09C0\
  \u09AF\u09BC \u09AA\u0995\u09CD\u09B7\u09C7\u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\
  \u09B0\u09C7\u09B0\u09BF\u0997\u09C1\u09B2\u09BF\u09B0 \u09AA\u09CD\u09B0\u09AF\u09BC\
  \u09CB\u099C\u09A8 \u099B\u09BE\u09A1\u09BC\u09BE\u0987 \u098F\u09B0 \u0985\u09AD\
  \u09CD\u09AF\u09A8\u09CD\u09A4\u09B0\u09C0\u09A3 \u09AB\u09BE\u0982\u09B6\u09A8\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09B8\u09CD\u099F\
  \u09CD\u09B0\u09BF\u0982\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u0995\u09CD\u09AF\u09BE\
  \u09AA\u09BF\u099F\u09BE\u09B2\u09BE\u0987\u099C \u0995\u09B0\u09BE\u09B0 \u098F\
  \u0995\u099F\u09BE \u09B8\u09CB\u099C\u09BE \u0989\u09AA\u09BE\u09AF\u09BC \u09AA\
  \u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964 \u098F\u0996\u09BE\u09A8\
  \u09C7 \u098F\u0995\u099F\u09BE \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3 \u0989\u09A6\
  \u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09AA\u09CD\u09B0\
  \u09A5\u09AE \u0985\u0995\u09CD\u09B7\u09B0 \u09AC\u09A1\u09BC \u09B9\u09BE\u09A4\
  \u09C7\u09B0 \u0995\u09B0\u09BE"
weight: 2
---

## কীভাবে:
এলিক্সির তৃতীয় পক্ষের লাইব্রেরিগুলির প্রয়োজন ছাড়াই এর অভ্যন্তরীণ ফাংশন ব্যবহার করে স্ট্রিংগুলিকে ক্যাপিটালাইজ করার একটা সোজা উপায় প্রদান করে। এখানে একটা সাধারণ উদাহরণ দেওয়া হল:

```elixir
string = "elixir programming"
capitalized_string = String.capitalize(string)
IO.puts capitalized_string
```

আউটপুট:

```
Elixir programming
```

যেখানে আরও নিয়ন্ত্রণ অথবা জটিল ক্যাপিটালাইজেশন লজিকের প্রয়োজন, আপনি ভিন্ন String ফাংশনগুলি সংমিশ্রণ করতে পারেন। উদাহরণ স্বরূপ, আপনি যদি চান যে একটি বাক্যের প্রতিটি শব্দ ক্যাপিটালাইজ করতে হবে, আপনি বাক্যটিকে শব্দে শব্দে বিভক্ত করতে পারেন, প্রতিটি শব্দ ক্যাপিটালাইজ করতে পারেন এবং এরপর তাদেরকে আবার একত্রিত করতে পারেন:

```elixir
sentence = "elixir is fun"
capitalized_sentence = sentence 
                        |> String.split() 
                        |> Enum.map(&String.capitalize/1) 
                        |> Enum.join(" ")

IO.puts capitalized_sentence
```

আউটপুট:

```
Elixir Is Fun
```

এলিক্সিরের স্ট্যান্ডার্ড লাইব্রেরি বেশিরভাগ চাহিদার জন্য যথেষ্ট, অবশ্য আর্থিক টেক্সট ম্যানিপুলেশনের জন্য, উন্নত স্ট্রিং ক্যাপিটালাইজেশন সহ, আপনি তৃতীয় পক্ষের লাইব্রেরিগুলি যেমন Cldr এর দিকে নজর দিতে পারেন, যা লোকাল-নির্দিষ্ট ক্যাপিটালাইজেশন আচরণ অফার করতে পারে।

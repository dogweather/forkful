---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:20.930492-06:00
description: "Elixir-\u098F \u098F\u0995\u099F\u09BF \u09A1\u09BF\u09B0\u09C7\u0995\
  \u09CD\u099F\u09B0\u09BF\u09B0 \u0985\u09B8\u09CD\u09A4\u09BF\u09A4\u09CD\u09AC\
  \ \u09AF\u09BE\u099A\u09BE\u0987 \u0995\u09B0\u09BE \u09B9\u09B2 \u09AB\u09BE\u0987\
  \u09B2 \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7 \u09A8\u09BF\u09B0\u09CD\
  \u09A6\u09BF\u09B7\u09CD\u099F \u09AA\u09A5\u09C7 \u098F\u0995\u099F\u09BF \u09A1\
  \u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF\u09B0 \u0989\u09AA\u09B8\u09CD\u09A5\
  \u09BF\u09A4\u09BF \u09AF\u09BE\u099A\u09BE\u0987 \u0995\u09B0\u09BE\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\
  \u09BF \u0995\u09B0\u09C7 \u09A8\u09BF\u09B6\u09CD\u099A\u09BF\u09A4 \u0995\u09B0\
  \u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09AF\u09C7\u2026"
lastmod: '2024-03-17T18:47:43.685960-06:00'
model: gpt-4-0125-preview
summary: "Elixir-\u098F \u098F\u0995\u099F\u09BF \u09A1\u09BF\u09B0\u09C7\u0995\u09CD\
  \u099F\u09B0\u09BF\u09B0 \u0985\u09B8\u09CD\u09A4\u09BF\u09A4\u09CD\u09AC \u09AF\
  \u09BE\u099A\u09BE\u0987 \u0995\u09B0\u09BE \u09B9\u09B2 \u09AB\u09BE\u0987\u09B2\
  \ \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\
  \u09BF\u09B7\u09CD\u099F \u09AA\u09A5\u09C7 \u098F\u0995\u099F\u09BF \u09A1\u09BF\
  \u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF\u09B0 \u0989\u09AA\u09B8\u09CD\u09A5\u09BF\
  \u09A4\u09BF \u09AF\u09BE\u099A\u09BE\u0987 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF\
  \ \u0995\u09B0\u09C7 \u09A8\u09BF\u09B6\u09CD\u099A\u09BF\u09A4 \u0995\u09B0\u09BE\
  \u09B0 \u099C\u09A8\u09CD\u09AF \u09AF\u09C7\u2026"
title: "\u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF \u0986\u099B\u09C7\
  \ \u0995\u09BF\u09A8\u09BE \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\
  \u09BE"
---

{{< edit_this_page >}}

## কী এবং কেন?
Elixir-এ একটি ডিরেক্টরির অস্তিত্ব যাচাই করা হল ফাইল সিস্টেমে নির্দিষ্ট পথে একটি ডিরেক্টরির উপস্থিতি যাচাই করা। প্রোগ্রামাররা এটি করে নিশ্চিত করার জন্য যে তারা নিরাপদে ডিরেক্টরিটি পড়তে, লিখতে অথবা অপারেশন সম্পাদন করতে পারে, এমনকি এর অনুপস্থিতির কারণে ত্রুটি মোকাবিলা না করে।

## কিভাবে:
Elixir-এর স্ট্যান্ডার্ড লাইব্রেরি `File` মডিউলের মাধ্যমে একটি ডিরেক্টরির অস্তিত্ব যাচাই করার একটি সরল উপায় প্রদান করে। এখানে আপনি কিভাবে এটি ব্যবহার করতে পারেন:

```elixir
if File.dir?("path/to/directory") do
  IO.puts "Directory exists!"
else
  IO.puts "Directory does not exist."
end
```

ধরুন ডিরেক্টরি অস্তিত্ব নেই, তাহলে নমুনা আউটপুট হবে:
```
Directory does not exist.
```

ডিরেক্টরির অস্তিত্ব যাচাই সহ আরও উন্নত ফাইলসিস্টেমের মিথস্ক্রিয়াগুলোর জন্য, আপনি `FileSystem`-এর মতো তৃতীয় পক্ষের লাইব্রেরিগুলি বিবেচনা করতে পারেন। যদিও Elixir-এর মান ক্ষমতা অনেক ক্ষেত্রে যথেষ্ট, `FileSystem` জটিল অ্যাপ্লিকেশনগুলির জন্য আরও সুনির্দিষ্ট নিয়ন্ত্রণ এবং প্রতিক্রিয়া অফার করতে পারে। তবে, যদি একটি ডিরেক্টরির অস্তিত্ব যাচাই করার মৌলিক প্রয়োজন থাকে, তাহলে সাধারণত নিজস্ব `File` মডিউলের দিকেই ঝুঁকতে বলা হয় কারণ এটি সহজলভ্য এবং কোনো বাইরের নির্ভরতা দাবি করে না।

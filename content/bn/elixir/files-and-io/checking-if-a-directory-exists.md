---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:20.930492-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Elixir-\u098F\u09B0 \u09B8\u09CD\
  \u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u09B2\u09BE\u0987\
  \u09AC\u09CD\u09B0\u09C7\u09B0\u09BF `File` \u09AE\u09A1\u09BF\u0989\u09B2\u09C7\
  \u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u098F\u0995\u099F\u09BF \u09A1\
  \u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF\u09B0 \u0985\u09B8\u09CD\u09A4\u09BF\
  \u09A4\u09CD\u09AC \u09AF\u09BE\u099A\u09BE\u0987 \u0995\u09B0\u09BE\u09B0 \u098F\
  \u0995\u099F\u09BF \u09B8\u09B0\u09B2 \u0989\u09AA\u09BE\u09AF\u09BC \u09AA\u09CD\
  \u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7\
  \ \u0986\u09AA\u09A8\u09BF \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u098F\u099F\u09BF\
  \u2026"
lastmod: '2024-03-17T18:47:43.685960-06:00'
model: gpt-4-0125-preview
summary: "Elixir-\u098F\u09B0 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\
  \u09BE\u09B0\u09CD\u09A1 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\
  \ `File` \u09AE\u09A1\u09BF\u0989\u09B2\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\
  \u09AE\u09C7 \u098F\u0995\u099F\u09BF \u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\
  \u09B0\u09BF\u09B0 \u0985\u09B8\u09CD\u09A4\u09BF\u09A4\u09CD\u09AC \u09AF\u09BE\
  \u099A\u09BE\u0987 \u0995\u09B0\u09BE\u09B0 \u098F\u0995\u099F\u09BF \u09B8\u09B0\
  \u09B2 \u0989\u09AA\u09BE\u09AF\u09BC \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\
  \u09B0\u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u0986\u09AA\u09A8\u09BF \u0995\
  \u09BF\u09AD\u09BE\u09AC\u09C7 \u098F\u099F\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8."
title: "\u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF \u0986\u099B\u09C7\
  \ \u0995\u09BF\u09A8\u09BE \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\
  \u09BE"
weight: 20
---

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

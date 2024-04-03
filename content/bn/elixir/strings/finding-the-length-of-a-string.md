---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:12.615749-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u098F\u09B2\u09BF\u0995\u09CD\
  \u09B8\u09BF\u09B0\u09C7, \u0986\u09AA\u09A8\u09BF `String.length/1` \u09AB\u09BE\
  \u0982\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \ \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0\
  \ \u09A6\u09C8\u09B0\u09CD\u0998\u09CD\u09AF \u09AA\u09C7\u09A4\u09C7 \u09AA\u09BE\
  \u09B0\u09C7\u09A8\u0964 \u098F\u099F\u09BF \u0995\u09BF\u09AD\u09BE\u09AC\u09C7\
  \ \u0995\u09B0\u09AC\u09C7\u09A8 \u09A4\u09BE \u09A8\u09C0\u099A\u09C7 \u09A6\u09C7\
  \u0993\u09AF\u09BC\u09BE \u09B9\u09B2."
lastmod: '2024-03-17T18:47:43.658221-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u09B2\u09BF\u0995\u09CD\u09B8\u09BF\u09B0\u09C7, \u0986\u09AA\u09A8\
  \u09BF `String.length/1` \u09AB\u09BE\u0982\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\
  \u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09A6\u09C8\u09B0\u09CD\u0998\u09CD\u09AF\
  \ \u09AA\u09C7\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u098F\u099F\u09BF\
  \ \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u0995\u09B0\u09AC\u09C7\u09A8 \u09A4\u09BE\
  \ \u09A8\u09C0\u099A\u09C7 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09A6\u09C8\u09B0\
  \u09CD\u0998\u09CD\u09AF \u099A\u09BF\u09B9\u09CD\u09A8\u09BF\u09A4 \u0995\u09B0\
  \u09BE"
weight: 7
---

## কিভাবে:
এলিক্সিরে, আপনি `String.length/1` ফাংশন ব্যবহার করে একটি স্ট্রিং এর দৈর্ঘ্য পেতে পারেন। এটি কিভাবে করবেন তা নীচে দেওয়া হল:

```elixir
my_string = "Hello, World!"
length = String.length(my_string)
IO.puts(length)
```

নমুনা আউটপুট:

```
13
```

## গভীর ডুব
অন্তরভাগে, এলিক্সির স্ট্রিংগুলি UTF-8 কোডিত বাইনারিজ হিসেবে থাকে। প্রতিটি অক্ষর এক থেকে চার বাইট পর্যন্ত হতে পারে। তাই, আমরা `String.length/1` কল করলে, আমরা কেবল বাইট গণনা করি না; আমরা ইউনিকোড গ্রাফিমস গণনা করি, যা আমরা অক্ষর হিসেবে অনুভব করি।

ঐতিহাসিকভাবে, অনেক ভাষায় স্ট্রিং দৈর্ঘ্য অপারেশনগুলি বাইট-কেন্দ্রিক ছিল এবং এটি বহু-বাইট অক্ষরের জন্য ভালভাবে বিবেচনা করেনি। এলিক্সিরের দৃষ্টিভঙ্গি আধুনিক এবং ইউনিকোড-বান্ধব শুরু থেকেই।

এর বিকল্প হিসেবে, আপনি ম্যানুয়ালি গ্রাফিমস গণনা করতে পারেন রিকার্সন বা লুপ ব্যবহার করে, কিন্তু তা অপ্রয়োজনীয় ও অকার্যকর। `String.length/1` ইতিমধ্যেই অনুকূলিত এবং উপযুক্ত।

এলিক্সিরের বাস্তবায়নে Erlang NIF (Native Implemented Function) ব্যবহার করা হয় `String.length/1` এর জন্য, যা এটিকে দ্রুতগতি প্রদান করে। গ্রাফিমসের পরিবর্তে বাইট গণনা করা হয় `byte_size/1` দ্বারা, যা স্ট্রিং এর বাইনারি প্রতিনিধিত্বের কাঁচা বাইটগুলি গণনা করে—যা নিম্নস্তরের অপারেশনে উপযোগী যেখানে এনকোডিং গুরুত্বপূর্ণ নয়।

## দেখুন ও
- [এলিক্সিরের স্ট্রিং মডিউলের ডকুমেন্টেশন](https://hexdocs.pm/elixir/String.html)
- [ইউনিকোড স্ট্যান্ডার্ড](http://www.unicode.org/standard/standard.html)

---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:55.873237-06:00
description: "\u0995\u09CB\u09A1\u0995\u09C7 \u09AB\u09BE\u0982\u09B6\u09A8\u0997\u09C1\
  \u09B2\u09BF\u09A4\u09C7 \u0986\u09AF\u09BC\u09CB\u099C\u09A8 \u0995\u09B0\u09BE\
  \ \u09AE\u09BE\u09A8\u09C7 \u09B8\u09AE\u09CD\u09AA\u09B0\u09CD\u0995\u09BF\u09A4\
  \ \u0985\u09AA\u09BE\u09B0\u09C7\u09B6\u09A8\u0997\u09C1\u09B2\u09BF\u0995\u09C7\
  \ \u09AA\u09C1\u09A8\u0983\u09AC\u09CD\u09AF\u09AC\u09B9\u09C3\u09A4 \u09AC\u09CD\
  \u09B2\u0995\u0997\u09C1\u09B2\u09BF\u09A4\u09C7 \u09AD\u09BE\u0997 \u0995\u09B0\
  \u09BE\u0964 \u0986\u09AE\u09B0\u09BE \u098F\u099F\u09BF \u09AA\u09A0\u09A8\u09AF\
  \u09CB\u0997\u09CD\u09AF\u09A4\u09BE \u098F\u09AC\u0982 \u09B0\u0995\u09CD\u09B7\
  \u09A3\u09BE\u09AC\u09C7\u0995\u09CD\u09B7\u09A3\u09C7\u09B0 \u0989\u09A8\u09CD\u09A8\
  \u09A4\u09BF, \u09A1\u09C1\u09AA\u09CD\u09B2\u09BF\u0995\u09C7\u09B6\u09A8 \u09B9\
  \u09CD\u09B0\u09BE\u09B8,\u2026"
lastmod: '2024-03-17T18:47:43.675996-06:00'
model: gpt-4-0125-preview
summary: "\u0995\u09CB\u09A1\u0995\u09C7 \u09AB\u09BE\u0982\u09B6\u09A8\u0997\u09C1\
  \u09B2\u09BF\u09A4\u09C7 \u0986\u09AF\u09BC\u09CB\u099C\u09A8 \u0995\u09B0\u09BE\
  \ \u09AE\u09BE\u09A8\u09C7 \u09B8\u09AE\u09CD\u09AA\u09B0\u09CD\u0995\u09BF\u09A4\
  \ \u0985\u09AA\u09BE\u09B0\u09C7\u09B6\u09A8\u0997\u09C1\u09B2\u09BF\u0995\u09C7\
  \ \u09AA\u09C1\u09A8\u0983\u09AC\u09CD\u09AF\u09AC\u09B9\u09C3\u09A4 \u09AC\u09CD\
  \u09B2\u0995\u0997\u09C1\u09B2\u09BF\u09A4\u09C7 \u09AD\u09BE\u0997 \u0995\u09B0\
  \u09BE\u0964 \u0986\u09AE\u09B0\u09BE \u098F\u099F\u09BF \u09AA\u09A0\u09A8\u09AF\
  \u09CB\u0997\u09CD\u09AF\u09A4\u09BE \u098F\u09AC\u0982 \u09B0\u0995\u09CD\u09B7\
  \u09A3\u09BE\u09AC\u09C7\u0995\u09CD\u09B7\u09A3\u09C7\u09B0 \u0989\u09A8\u09CD\u09A8\
  \u09A4\u09BF, \u09A1\u09C1\u09AA\u09CD\u09B2\u09BF\u0995\u09C7\u09B6\u09A8 \u09B9\
  \u09CD\u09B0\u09BE\u09B8, \u098F\u09AC\u0982 \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\
  \u09BE \u09B8\u09B9\u099C \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u0995\
  \u09B0\u09BF\u0964."
title: "\u0995\u09CB\u09A1\u0995\u09C7 \u09AB\u09BE\u0982\u09B6\u09A8\u0997\u09C1\u09B2\
  \u09BF\u09A4\u09C7 \u0986\u09AF\u09BC\u09CB\u099C\u09A8 \u0995\u09B0\u09BE"
weight: 18
---

## কি এবং কেন?
কোডকে ফাংশনগুলিতে আয়োজন করা মানে সম্পর্কিত অপারেশনগুলিকে পুনঃব্যবহৃত ব্লকগুলিতে ভাগ করা। আমরা এটি পঠনযোগ্যতা এবং রক্ষণাবেক্ষণের উন্নতি, ডুপ্লিকেশন হ্রাস, এবং পরীক্ষা সহজ করার জন্য করি।

## কিভাবে:
চলুন শব্দগুলিকে বড় হাতের অক্ষরে লিখার জন্য একটি সাধারণ এলিক্সির ফাংশন তৈরি করি:

```elixir
defmodule StringUtils do
  def capitalize_words(sentence) do
    sentence
    |> String.split()
    |> Enum.map(&String.capitalize/1)
    |> Enum.join(" ")
  end
end

IO.puts StringUtils.capitalize_words("hello elixir world")
```
আউটপুট:
```
Hello Elixir World
```
এখানে, আমরা শব্দের বড় হাতের অক্ষরে লিখার লজিককে সুন্দর ভাবে `capitalize_words` নামে একটি ফাংশনে প্যাক করেছি।

## গভীর ডুব
এলিক্সিরে, এবং বৃহত্তর এরল্যাং VM ইকোসিস্টেমে, ফাংশনগুলি প্রথম শ্রেণীর নাগরিক, সমস্যাগুলিকে ছোট, পরিচালনাযোগ্য, এবং আলাদা টুকরাতে ভেঙ্গে দেওয়ার দর্শন গ্রহণ করে। ঐতিহাসিকভাবে, এই ফাংশনাল পদ্ধতির লাম্বডা ক্যালকুলাস এবং লিস্পসে শিকড়, কোড হিসেবে ডাটা দর্শন প্রচার করে।

কোডকে আয়োজনের বিকল্পগুলি ম্যাক্রো বা এলিক্সিরের প্রসেসগুলিকে পুনরাবৃত্তি বা সমান্তরাল টাস্কের জন্য ব্যবহার করা যেতে পারে। বাস্তবায়নের দিক থেকে, এলিক্সিরের ফাংশনগুলি প্যাটার্ন ম্যাচিং সামলাতে এবং বিভিন্ন ধরনের আর্গুমেন্টের (এরিটি) গ্রহণ করতে পারে, তাদের বহুমুখিতা দান করে।

## আরও দেখুন
- [এলিক্সিরের অফিসিয়াল ডকুমেন্টেশন অন ফাংশনস](https://hexdocs.pm/elixir/Kernel.html#functions)
- [ডেভ থমাসের "প্রোগ্রামিং এলিক্সির"](https://pragprog.com/titles/elixir16/programming-elixir-1-6/)

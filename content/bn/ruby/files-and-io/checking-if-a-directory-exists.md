---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:32.119256-06:00
description: "\u09B0\u09C1\u09AC\u09BF \u09AD\u09BE\u09B7\u09BE\u09AF\u09BC \u098F\
  \u0995\u099F\u09BF \u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF\u09B0\
  \ \u0985\u09B8\u09CD\u09A4\u09BF\u09A4\u09CD\u09AC \u09AA\u09B0\u09C0\u0995\u09CD\
  \u09B7\u09BE \u0995\u09B0\u09BE, \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\
  \u09AE\u09BE\u09B0\u09A6\u09C7\u09B0 \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\u09BC\
  \u09BE \u0985\u09A5\u09AC\u09BE \u09A8\u09A4\u09C1\u09A8 \u09A1\u09BF\u09B0\u09C7\
  \u0995\u09CD\u099F\u09B0\u09BF \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE\u09B0\
  \ \u09AE\u09A4 \u0985\u09AA\u09BE\u09B0\u09C7\u09B6\u09A8 \u0997\u09C1\u09B2\u09BF\
  \ \u09B8\u09AE\u09CD\u09AA\u09BE\u09A6\u09A8\u09C7\u09B0 \u0986\u0997\u09C7 \u09A1\
  \u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF\u09B0 \u0989\u09AA\u09B8\u09CD\u09A5\
  \u09BF\u09A4\u09BF\u2026"
lastmod: '2024-03-17T18:47:44.605882-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09C1\u09AC\u09BF \u09AD\u09BE\u09B7\u09BE\u09AF\u09BC \u098F\u0995\
  \u099F\u09BF \u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF\u09B0 \u0985\
  \u09B8\u09CD\u09A4\u09BF\u09A4\u09CD\u09AC \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE\
  \ \u0995\u09B0\u09BE, \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\
  \u09B0\u09A6\u09C7\u09B0 \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\u09BC\u09BE \u0985\
  \u09A5\u09AC\u09BE \u09A8\u09A4\u09C1\u09A8 \u09A1\u09BF\u09B0\u09C7\u0995\u09CD\
  \u099F\u09B0\u09BF \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE\u09B0 \u09AE\u09A4\
  \ \u0985\u09AA\u09BE\u09B0\u09C7\u09B6\u09A8 \u0997\u09C1\u09B2\u09BF \u09B8\u09AE\
  \u09CD\u09AA\u09BE\u09A6\u09A8\u09C7\u09B0 \u0986\u0997\u09C7 \u09A1\u09BF\u09B0\
  \u09C7\u0995\u09CD\u099F\u09B0\u09BF\u09B0 \u0989\u09AA\u09B8\u09CD\u09A5\u09BF\u09A4\
  \u09BF\u2026"
title: "\u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF \u0986\u099B\u09C7\
  \ \u0995\u09BF\u09A8\u09BE \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\
  \u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
রুবি ভাষায় একটি ডিরেক্টরির অস্তিত্ব পরীক্ষা করা, প্রোগ্রামারদের ফাইল পড়া অথবা নতুন ডিরেক্টরি তৈরি করার মত অপারেশন গুলি সম্পাদনের আগে ডিরেক্টরির উপস্থিতি যাচাই করতে সাহায্য করে। ফাইল হ্যান্ডলিং-এ ত্রুটি এড়াতে এবং ফাইল সিস্টেমের মণিপুলেশনের বিশ্বাসযোগ্যতা নিশ্চিত করার জন্য এটি গুরুত্বপূর্ণ।

## কিভাবে:
রুবির স্ট্যান্ডার্ড লাইব্রেরি একটি ডিরেক্টরির অস্তিত্ব পরীক্ষা করার জন্য সোজা সাপটা পদ্ধতি প্রদান করে। এখানে এটি কিভাবে শুধুমাত্র রুবি ব্যবহার করে করা যায়, কোনো তৃতীয়-পক্ষের লাইব্রেরির প্রয়োজন ছাড়া:

```ruby
require 'fileutils'

# একটি ডিরেক্টরির অস্তিত্ব পরীক্ষা করা হচ্ছে
if Dir.exist?('/path/to/directory')
  puts 'ডিরেক্টরি বিদ্যমান।'
else
  puts 'ডিরেক্টরি বিদ্যমান নেই।'
end
```
নমুনা আউটপুট:
```
ডিরেক্টরি বিদ্যমান।
```
অথবা:
```
ডিরেক্টরি বিদ্যমান নেই।
```

`Dir.exist?` ব্যবহার করার পাশাপাশি, আপনি `File.directory?` মেথডটিও ব্যবহার করতে পারেন যা দেওয়া পাথটি একটি ডিরেক্টরি হলে `true` রিটার্ন করে:

```ruby
if File.directory?('/path/to/directory')
  puts 'ডিরেক্টরি বিদ্যমান।'
else
  puts 'ডিরেক্টরি বিদ্যমান নেই।'
end
```
`Dir.exist?` এবং `File.directory?` উভয়ই রুবির স্ট্যান্ডার্ড লাইব্রেরির অংশ এবং ব্যবহারের জন্য কোনো বাহ্যিক গেমসের প্রয়োজন নেই, ডিরেক্টরি পরীক্ষার জন্য এগুলি সুবিধাজনক এবং কার্যকরী বিকল্প।

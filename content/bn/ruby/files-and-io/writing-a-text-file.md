---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:40:23.321657-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B0\u09C1\u09AC\u09BF \u09AB\
  \u09BE\u0987\u09B2 \u0985\u09AA\u09BE\u09B0\u09C7\u09B6\u09A8\u0997\u09C1\u09B2\u09BF\
  \ \u09B8\u09CB\u099C\u09BE \u0995\u09B0\u09C7 \u09A4\u09CB\u09B2\u09C7\u0964 \u098F\
  \u0995\u099F\u09BF \u09AB\u09BE\u0987\u09B2\u09C7 \u09B2\u09BF\u0996\u09A4\u09C7\
  , \u0986\u09AA\u09A8\u09BF \u09B0\u09C1\u09AC\u09BF\u09B0 \u09A8\u09BF\u09B0\u09CD\
  \u09AE\u09BF\u09A4 `File` \u0995\u09CD\u09B2\u09BE\u09B8 \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964\
  \ \u09A8\u09BF\u09AE\u09CD\u09A8\u09B2\u09BF\u0996\u09BF\u09A4 \u0989\u09A6\u09BE\
  \u09B9\u09B0\u09A3\u09C7 \u09A6\u09C7\u0996\u09BE\u09A8\u09CB \u09B9\u09DF\u09C7\
  \u099B\u09C7 \u0995\u09BF\u09AD\u09BE\u09AC\u09C7\u2026"
lastmod: '2024-03-17T18:47:44.609923-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09C1\u09AC\u09BF \u09AB\u09BE\u0987\u09B2 \u0985\u09AA\u09BE\u09B0\
  \u09C7\u09B6\u09A8\u0997\u09C1\u09B2\u09BF \u09B8\u09CB\u099C\u09BE \u0995\u09B0\
  \u09C7 \u09A4\u09CB\u09B2\u09C7\u0964 \u098F\u0995\u099F\u09BF \u09AB\u09BE\u0987\
  \u09B2\u09C7 \u09B2\u09BF\u0996\u09A4\u09C7, \u0986\u09AA\u09A8\u09BF \u09B0\u09C1\
  \u09AC\u09BF\u09B0 \u09A8\u09BF\u09B0\u09CD\u09AE\u09BF\u09A4 `File` \u0995\u09CD\
  \u09B2\u09BE\u09B8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\
  \u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u09A8\u09BF\u09AE\u09CD\u09A8\u09B2\
  \u09BF\u0996\u09BF\u09A4 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3\u09C7 \u09A6\u09C7\
  \u0996\u09BE\u09A8\u09CB \u09B9\u09DF\u09C7\u099B\u09C7 \u0995\u09BF\u09AD\u09BE\
  \u09AC\u09C7 \u09B2\u09C7\u0996\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\
  \u099F\u09BF \u09AB\u09BE\u0987\u09B2 \u0993\u09AA\u09C7\u09A8 \u0995\u09B0\u09A4\
  \u09C7 \u09B9\u09DF (`\"w\"` \u09AE\u09CB\u09A1) \u098F\u09AC\u0982 \u09B8\u0982\
  \u09AF\u09C1\u0995\u09CD\u09A4 \u0995\u09B0\u09BE (`\"a\"` \u09AE\u09CB\u09A1),\
  \ \u09A4\u09BE\u09B0\u09AA\u09B0\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\
  \u09CD\u09B0\u09BF\u0982 \u09B2\u09BF\u0996\u09C7, \u098F\u09AC\u0982 \u09AA\u09B0\
  \u09C7 \u09AB\u09BE\u0987\u09B2\u099F\u09BF \u09AC\u09A8\u09CD\u09A7 \u0995\u09B0\
  \u09C7 \u09A6\u09BF\u09A4\u09C7 \u09A8\u09BF\u09B6\u09CD\u099A\u09BF\u09A4 \u0995\
  \u09B0\u09BE\u0983."
title: "\u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\
  \u0987\u09B2 \u09B2\u09BF\u0996\u09BE"
weight: 24
---

## কিভাবে:
রুবি ফাইল অপারেশনগুলি সোজা করে তোলে। একটি ফাইলে লিখতে, আপনি রুবির নির্মিত `File` ক্লাস ব্যবহার করতে পারেন। নিম্নলিখিত উদাহরণে দেখানো হয়েছে কিভাবে লেখার জন্য একটি ফাইল ওপেন করতে হয় (`"w"` মোড) এবং সংযুক্ত করা (`"a"` মোড), তারপরে একটি স্ট্রিং লিখে, এবং পরে ফাইলটি বন্ধ করে দিতে নিশ্চিত করাঃ

```ruby
# একটি ফাইলে নতুন কন্টেন্ট লিখে, বিদ্যমান কন্টেন্ট রিপ্লেস করে
File.open("example.txt", "w") do |file|
  file.puts "Hello, Ruby!"
end

# একটি ফাইলের শেষে কন্টেন্ট যোগ করা
File.open("example.txt", "a") do |file|
  file.puts "Adding another line."
end
```
উভয় স্নিপেট রান করার পর, `example.txt` এর কন্টেন্ট হবেঃ
```
Hello, Ruby!
Adding another line.
```

### থার্ড-পার্টি লাইব্রেরি ব্যবহারঃ FileUtils
আরো জটিল ফাইল অপারেশনের জন্য, রুবি স্ট্যান্ডার্ড লাইব্রেরি `FileUtils` সাহায্যকারী হতে পারে, যদিও মৌলিক ফাইল লিখনের জন্য স্ট্যান্ডার্ড `File` পদ্ধতিগুলি যথেষ্ট। তবে, যদি আপনি কপি, সরান, মুছে ফেলা, অথবা ফাইল লিখনের সাথে অন্য ফাইলসিস্টেম অপারেশন সঞ্চালন করতে চান, তাহলে `FileUtils` অন্বেষণ করা মূল্যবান।

`FileUtils` ব্যবহার করে একটি ডিরেক্টরি তৈরি করে তার মধ্যে ফাইলে লিখার একটি উদাহরণঃ
```ruby
require 'fileutils'

FileUtils.mkdir_p 'logs'
File.open("logs/today.log", "w") do |file|
  file.puts "Log entry: #{Time.now}"
end
```

এটি দেখায় যে, যদি আগে থেকে না থাকে তবে নতুন একটি ডিরেক্টরি `logs` তৈরি করে, এবং এর মধ্যে একটি নতুন ফাইল `today.log` লিখে, ফোল্ডার এবং ফাইল ম্যানিপুলেশন দেখায়, সরাসরি FileUtils দিয়ে লিখন ছাড়াই কিন্তু এর ডিরেক্টরি হ্যান্ডলিং ক্ষমতা ব্যবহার করে।

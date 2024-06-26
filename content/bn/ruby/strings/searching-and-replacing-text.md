---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:16:55.494137-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B0\u09C1\u09AC\u09BF \u098F\
  \u099F\u09BF \u09B8\u09B9\u099C \u0995\u09B0\u09C7 \u09A6\u09C7\u09DF\u0964 \u0997\
  \u09CD\u09B2\u09CB\u09AC\u09BE\u09B2\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F\
  \ \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\u09A5\u09BE\u09AA\u09A8\u09C7\u09B0\
  \ \u099C\u09A8\u09CD\u09AF `gsub` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09C1\u09A8, \u0985\u09A5\u09AC\u09BE \u098F\u0995\u0995 \u0989\u09A6\u09BE\
  \u09B9\u09B0\u09A3\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF `sub`\u0964 \u098F\u0996\
  \u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09A6\u09CD\u09B0\u09C1\u09A4 \u09A6\
  \u09C3\u09B7\u09CD\u099F\u09BE\u09A8\u09CD\u09A4 \u09A6\u09C7\u0996\u09BE \u09AF\
  \u09BE\u0995."
lastmod: '2024-03-17T18:47:44.571825-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09C1\u09AC\u09BF \u098F\u099F\u09BF \u09B8\u09B9\u099C \u0995\u09B0\
  \u09C7 \u09A6\u09C7\u09DF\u0964 \u0997\u09CD\u09B2\u09CB\u09AC\u09BE\u09B2\u09BF\
  \ \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\
  \u09A5\u09BE\u09AA\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF `gsub` \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C1\u09A8, \u0985\u09A5\u09AC\u09BE\
  \ \u098F\u0995\u0995 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3\u09C7\u09B0 \u099C\u09A8\
  \u09CD\u09AF `sub`\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF\
  \ \u09A6\u09CD\u09B0\u09C1\u09A4 \u09A6\u09C3\u09B7\u09CD\u099F\u09BE\u09A8\u09CD\
  \u09A4 \u09A6\u09C7\u0996\u09BE \u09AF\u09BE\u0995."
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\u09A7\
  \u09BE\u09A8 \u098F\u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\u09A5\
  \u09BE\u09AA\u09A8"
weight: 10
---

## কিভাবে:
রুবি এটি সহজ করে দেয়। গ্লোবালি টেক্সট প্রতিস্থাপনের জন্য `gsub` ব্যবহার করুন, অথবা একক উদাহরণের জন্য `sub`। এখানে একটি দ্রুত দৃষ্টান্ত দেখা যাক:

```ruby
# মূল স্ট্রিং
phrase = "Hello, world!"

# 'world' কে 'Ruby' দিয়ে প্রতিস্থাপন করা
puts phrase.gsub('world', 'Ruby')
# => Hello, Ruby!

# 'l' এর প্রথম উদাহরণটি প্রতিস্থাপন করা
puts phrase.sub('l', '7')
# => He7lo, world!
```
আউটপুট কি? প্রথম প্রিন্টটি `"Hello, Ruby!"` দেখায়, দ্বিতীয়টি `"He7lo, world!"` দেখায়।

## গভীর ডুব
`gsub` এবং `sub` পদ্ধতিগুলো রুবির প্রাথমিক দিন থেকেই আছে, পার্লের মতো পুরানো ভাষাগুলির প্রতিস্থাপন ধারণাটি প্রতিবিম্বিত করে। বিকল্প? নিশ্চিতভাবেই, আপনি জটিল মানচিত্রাংশের জন্য একটি রেজেক্স ব্যবহার করতে পারেন, অথবা এমনকি `split` এবং `join` একসাথে মিলাতে হলে যদি আপনি সৃজনশীলতার মুডে থাকেন।

কি দারুণ বিষয় হল, রুবির `gsub` এর সাথে ব্লকের সক্ষমতা। একটি সাধারণ খুঁজে পেতে ও প্রতিস্থাপন করতের বদলে, আপনি সেই ব্লকের ভিতরে কিছু ভারী কাজ করতে পারেন:

```ruby
# প্রতিটি শব্দকে মূলাক্ষরে প্রকাশ করা
puts "make me pretty".gsub(/\b\w/) { |match| match.upcase }
# => Make Me Pretty
```

কেন এটা নিয়ে ঝামেলা? সূচনা হিসেবে, `gsub`-এর সাথে রেজেক্স ব্যবহার করে আপনি নুন্যতম কেসগুলিতে যেখানে আপনার বেশী সূক্ষ্মতা প্রয়োজন হতে পারে সেগুলি সম্ভালতে পারেন।

## আরও দেখুন
সেই দক্ষতাগুলি প্রখর করুন - ডকুমেন্টেশনে ডুব দিন অথবা এই সম্পদগুলি পরীক্ষা করুন:
- [রুবি String#gsub ডকুমেন্টেশন](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)
- [রুবিতে রেগুলার এক্সপ্রেশন](https://www.rubyguides.com/2015/06/ruby-regex/)

বুঝেছেন? ভালো। এবার কিছু স্ট্রিং নিয়ে খেলা করুন।

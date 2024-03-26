---
changelog:
- 2024-03-25, dogweather, edited and tested
- 2024-03-25, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:29.358527-07:00
description: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09DF\
  \u09C7\u09B0 \u09AA\u09CD\u09B0\u09A5\u09AE \u0985\u0995\u09CD\u09B7\u09B0\u0995\
  \u09C7 \u09AC\u09A1\u09BC \u09B9\u09BE\u09A4\u09C7\u09B0 \u0985\u0995\u09CD\u09B7\
  \u09B0\u09C7 (uppercase) \u098F\u09AC\u0982 \u09AC\u09BE\u0995\u09C0 \u0985\u0995\
  \u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09CB\u0995\u09C7 \u099B\u09CB\u099F \u09B9\
  \u09BE\u09A4\u09C7\u09B0 \u0985\u0995\u09CD\u09B7\u09B0\u09C7 (lowercase) \u09B0\
  \u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE\u0995\u09C7 \u09B8\
  \u09BE\u09A7\u09BE\u09B0\u09A3\u09A4 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\
  \u09CD\u09B0\u09BF\u0982 \u09AE\u09C2\u09B2\u09A7\u09B0\u09A8\u09C7\u09B0\u2026"
lastmod: '2024-03-25T19:25:21.868341-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09DF\
  \u09C7\u09B0 \u09AA\u09CD\u09B0\u09A5\u09AE \u0985\u0995\u09CD\u09B7\u09B0\u0995\
  \u09C7 \u09AC\u09A1\u09BC \u09B9\u09BE\u09A4\u09C7\u09B0 \u0985\u0995\u09CD\u09B7\
  \u09B0\u09C7 (uppercase) \u098F\u09AC\u0982 \u09AC\u09BE\u0995\u09C0 \u0985\u0995\
  \u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09CB\u0995\u09C7 \u099B\u09CB\u099F \u09B9\
  \u09BE\u09A4\u09C7\u09B0 \u0985\u0995\u09CD\u09B7\u09B0\u09C7 (lowercase) \u09B0\
  \u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE\u0995\u09C7 \u09B8\
  \u09BE\u09A7\u09BE\u09B0\u09A3\u09A4 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\
  \u09CD\u09B0\u09BF\u0982 \u09AE\u09C2\u09B2\u09A7\u09B0\u09A8\u09C7\u09B0\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AC\u09A1\u09BC \u09B9\u09BE\
  \u09A4\u09C7\u09B0 \u0985\u0995\u09CD\u09B7\u09B0\u09C7 \u09AA\u09B0\u09BF\u09A3\
  \u09A4 \u0995\u09B0\u09BE"
---

## কি এবং কেন?
একটি স্ট্রিংয়ের প্রথম অক্ষরকে বড় হাতের অক্ষরে (uppercase) এবং বাকী অক্ষরগুলোকে ছোট হাতের অক্ষরে (lowercase) রূপান্তর করাকে সাধারণত একটি স্ট্রিং মূলধরনের (capitalize) বলা হয়। কিন্তু কখনো কখনো এর মানে হতে পারে শুধুমাত্র নিশ্চিত করা যে প্রথম অক্ষরটি বড় হাতের অক্ষর হয়, বাকিটা অপরিবর্তিত রাখা। সত্যি বলতে, আমার মতে, এটি একটি কিছুটা অস্পষ্ট শব্দ।

## কীভাবে:
Ruby প্রদান করে [সরল পদ্ধতিগুলি স্ট্রিং ম্যানিপুলেশনের জন্য](https://docs.ruby-lang.org/en/3.3/String.html), যার মধ্যে রয়েছে মূলধরন:

```ruby
# Ruby'র বিল্ট-ইন মেথড
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

খুবই সুবিধাজনক।

Ruby'র `.capitalize` মেথড সুবিধাজনক ঠিকই, কিন্তু এটি শুধু প্রথম অক্ষরটিকেই বড় করে। আরও নিয়ন্ত্রণ বা একটি স্ট্রিংয়ের প্রতিটি শব্দকে মূলধরনের জন্য (যা শিরোনাম কেইস নামে পরিচিত), আপনি Rails ActiveSupport এক্সটেনশনের `titleize` মেথড ব্যবহার করতে পারেন, অথবা নিজেই এটি বাস্তবায়ন করতে পারেন:

```ruby
# Rails-এ ActiveSupport'র 'titleize' ব্যবহার করে
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# একটি নিজস্ব সমাধান
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

এই পদ্ধতিটি স্ট্রিংটিকে শব্দের একটি অ্যারেতে ভাগ করে, প্রতিটি শব্দকে মূলধরন করে, এরপর তাদের একটি স্থানের সাথে পুনরায় একত্রিত করে।

ব্যক্তিগতভাবে, আমি আমার কোডে এই ধারণাটি অনেক দূরের পথ নিয়ে যাই। আমি নিজের [`titleize` মেথডটি লিখেছি যা "a" এবং "the" এর মতো ছোট শব্দগুলোকে গ্রহণ করে](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).

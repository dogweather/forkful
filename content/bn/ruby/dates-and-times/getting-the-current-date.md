---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:30.744837-06:00
description: "\u09AA\u09CD\u09B0\u09BE\u09DF \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BF\u0982 \u0989\u09A6\u09CD\u09AF\u09CB\u0997\u09C7\u09B0 \u09AE\
  \u09A7\u09CD\u09AF\u09C7 \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\
  \u09B0\u09BF\u0996 \u09AA\u09C7\u09A4\u09C7 \u09AF\u09BE\u0993\u09DF\u09BE \u098F\
  \u0995\u099F\u09BF \u0985\u09AA\u09B0\u09BF\u09B9\u09BE\u09B0\u09CD\u09AF \u0995\
  \u09BE\u099C, \u0985\u09CD\u09AF\u09BE\u09AA\u09CD\u09B2\u09BF\u0995\u09C7\u09B6\
  \u09A8\u09C7 \u0995\u09BE\u09B0\u09CD\u09AF\u0995\u09B2\u09BE\u09AA \u09B2\u0997\
  \ \u0995\u09B0\u09BE \u09A5\u09C7\u0995\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\
  \u09B0 \u09B8\u09AE\u09DF\u09B0\u09C7\u0996\u09BE \u09B8\u09B9 \u09B0\u09BF\u09AA\
  \u09CB\u09B0\u09CD\u099F \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE\u2026"
lastmod: '2024-03-17T18:47:44.601569-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09CD\u09B0\u09BE\u09DF \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BF\u0982 \u0989\u09A6\u09CD\u09AF\u09CB\u0997\u09C7\u09B0 \u09AE\
  \u09A7\u09CD\u09AF\u09C7 \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\
  \u09B0\u09BF\u0996 \u09AA\u09C7\u09A4\u09C7 \u09AF\u09BE\u0993\u09DF\u09BE \u098F\
  \u0995\u099F\u09BF \u0985\u09AA\u09B0\u09BF\u09B9\u09BE\u09B0\u09CD\u09AF \u0995\
  \u09BE\u099C, \u0985\u09CD\u09AF\u09BE\u09AA\u09CD\u09B2\u09BF\u0995\u09C7\u09B6\
  \u09A8\u09C7 \u0995\u09BE\u09B0\u09CD\u09AF\u0995\u09B2\u09BE\u09AA \u09B2\u0997\
  \ \u0995\u09B0\u09BE \u09A5\u09C7\u0995\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\
  \u09B0 \u09B8\u09AE\u09DF\u09B0\u09C7\u0996\u09BE \u09B8\u09B9 \u09B0\u09BF\u09AA\
  \u09CB\u09B0\u09CD\u099F \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE\u2026"
title: "\u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u09AA\u09C7\u09A4\u09C7"
weight: 29
---

## কি এবং কেন?
প্রায় প্রোগ্রামিং উদ্যোগের মধ্যে বর্তমান তারিখ পেতে যাওয়া একটি অপরিহার্য কাজ, অ্যাপ্লিকেশনে কার্যকলাপ লগ করা থেকে তারিখের সময়রেখা সহ রিপোর্ট তৈরি করা পর্যন্ত। Ruby-তে এটি স্ট্যান্ডার্ড লাইব্রেরি ব্যবহার করে সহজেই সম্পন্ন করা যায়, যা তারিখ সংক্রান্ত অপারেশনগুলি সহজ করে।

## কিভাবে:
Ruby-এর স্ট্যান্ডার্ড লাইব্রেরিতে তারিখ এবং সময় পরিচালনা করার জন্য `Date` এবং `Time` ক্লাস অন্তর্ভুক্ত রয়েছে। এখানে বর্তমান তারিখ পেতে হবে:

```ruby
require 'date'

current_date = Date.today
puts current_date
```

নমুনা আউটপুট: 
```
2023-04-12
```

তারিখের সাথে সময় অন্তর্ভুক্ত করতে, Ruby-এর `Time` ক্লাস আরও উপযুক্ত:

```ruby
current_time = Time.now
puts current_time
```

নমুনা আউটপুট: 
```
2023-04-12 14:33:07 +0200
```

আপনি যদি আরও কার্যকারিতা চান, যেমন সময় অঞ্চল ব্যবস্থাপনা, আপনি তৃতীয়-পক্ষের জেমের মতো `ActiveSupport` ব্যবহার করতে চাইতে পারেন (Rails-এর অংশ কিন্তু একা একাও ব্যবহৃত হতে পারে)।

প্রথমে, আপনার Gemfile-এ `activesupport` যোগ করুন এবং `bundle install` চালান:

```ruby
gem 'activesupport'
```

তারপর, সময় অঞ্চল সামলাতে এটি ব্যবহার করুন:

```ruby
require 'active_support/time'

Time.zone = 'Eastern Time (US & Canada)'  # আপনার কাঙ্খিত সময় অঞ্চল সেট করুন
current_time_with_zone = Time.zone.now
puts current_time_with_zone
```

নমুনা আউটপুট:
```
Wed, 12 Apr 2023 08:33:07 EDT -04:00
```

---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:30.744837-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Ruby-\u098F\u09B0 \u09B8\u09CD\
  \u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u09B2\u09BE\u0987\
  \u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u09A4\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u098F\u09AC\u0982 \u09B8\u09AE\u09AF\u09BC \u09AA\u09B0\u09BF\u099A\u09BE\u09B2\
  \u09A8\u09BE \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF `Date` \u098F\u09AC\
  \u0982 `Time` \u0995\u09CD\u09B2\u09BE\u09B8 \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\
  \u09AD\u09C1\u0995\u09CD\u09A4 \u09B0\u09DF\u09C7\u099B\u09C7\u0964 \u098F\u0996\
  \u09BE\u09A8\u09C7 \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\
  \u09BF\u0996 \u09AA\u09C7\u09A4\u09C7 \u09B9\u09AC\u09C7."
lastmod: '2024-03-17T18:47:44.601569-06:00'
model: gpt-4-0125-preview
summary: "Ruby-\u098F\u09B0 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\
  \u09BE\u09B0\u09CD\u09A1 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u09A4\
  \u09C7 \u09A4\u09BE\u09B0\u09BF\u0996 \u098F\u09AC\u0982 \u09B8\u09AE\u09AF\u09BC\
  \ \u09AA\u09B0\u09BF\u099A\u09BE\u09B2\u09A8\u09BE \u0995\u09B0\u09BE\u09B0 \u099C\
  \u09A8\u09CD\u09AF `Date` \u098F\u09AC\u0982 `Time` \u0995\u09CD\u09B2\u09BE\u09B8\
  \ \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09AD\u09C1\u0995\u09CD\u09A4 \u09B0\u09DF\
  \u09C7\u099B\u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u09AC\u09B0\u09CD\u09A4\
  \u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996 \u09AA\u09C7\u09A4\u09C7 \u09B9\
  \u09AC\u09C7."
title: "\u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u09AA\u09C7\u09A4\u09C7"
weight: 29
---

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

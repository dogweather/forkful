---
title:                "বর্তমান তারিখ পেতে"
date:                  2024-03-17T17:48:30.744837-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

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

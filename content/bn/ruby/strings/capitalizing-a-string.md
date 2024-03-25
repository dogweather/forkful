---
title:                "স্ট্রিং বড় হাতে লিখা"
date:                  2024-03-25T17:33:12.402754-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-25, dogweather, edited and tested
  - 2024-03-25, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
একটি স্ট্রিং অভিহিত করার ভাবটি সাধারণত স্ট্রিংটির প্রথম অক্ষরকে বড় হাতের অক্ষরে (uppercase) এবং বাকিগুলোকে ছোট হাতের অক্ষরে (lowercase) রূপান্তর করাকে বোঝায়। কিন্তু কখনও কখনও এর মানে হতে পারে শুধু নিশ্চিত করা যে প্রথম অক্ষরটি বড় হাতের অক্ষরে রূপান্তরিত হয়েছে এবং বাকি স্ট্রিংটি অপরিবর্তিত রাখা। সত্যি বলতে, আমার মতে, এটি একটি কিছুটা অস্পষ্ট শব্দ।

## কীভাবে:
রুবি স্ট্রিং ম্যানিপুলেশনের জন্য [সোজাসাপটা পদ্ধতি](https://docs.ruby-lang.org/en/3.3/String.html) প্রদান করে, যার মধ্যে অভিহিত করা অন্তর্ভুক্ত:

```ruby
# রুবির অন্তর্নির্মিত পদ্ধতি
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

খুবই সুবিধাজনক।

রুবির `.capitalize` পদ্ধতিটি সুবিধাজনক কিন্তু শুধু প্রথম অক্ষরটিকে বড় হাতের অক্ষরে পরিণত করে। আরও নিয়ন্ত্রণ পেতে বা একটি স্ট্রিংয়ের প্রতিটি শব্দের প্রথম অক্ষরকে বড় হাতের অক্ষরে পরিণত করতে (যাকে শিরোনাম কেস বলা হয়), আপনি রেলস অ্যাক্টিভ সাপোর্ট এক্সটেনশনের `titleize` পদ্ধতি ব্যবহার করতে পারেন অথবা নিজেই এটি বাস্তবায়ন করতে পারেন:

```ruby
# রেলসের অ্যাক্টিভ সাপোর্টের 'titleize' ব্যবহার করা
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# নিজস্ব সমাধান
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

এই পদ্ধতিটি স্ট্রিংটিকে শব্দের একটি অ্যারেতে বিভক্ত করে, প্রতিটি শব্দকে বড় হাতের অক্ষরে পরিণত করে এবং তারপর তাদের একটি স্পেসের সাথে পুনরায় জুড়ে দেয়।

ব্যক্তিগতভাবে, আমি এই ধারণাটিকে আমার কোডে অনেক দূরগামী নিয়ে যাই। আমি আমার নিজের [`titleize` পদ্ধতিটি লিখেছি যা "a" এবং "the" এর মত ছোট শব্দগুলিকে বিবেচনা করে](https://github.com/public-law/law_string/blob/master/lib/law_string.rb)।

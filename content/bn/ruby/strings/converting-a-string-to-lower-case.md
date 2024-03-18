---
title:                "স্ট্রিংকে লোয়ার কেসে রূপান্তর করা"
date:                  2024-03-17T17:46:39.764455-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

রুবিতে, একটি স্ট্রিংকে ছোট হাতের অক্ষরে পরিণত করার অর্থ হল স্ট্রিংটিতে থাকা সব বড় হাতের অক্ষরগুলিকে তাদের ছোট হাতের সমতুল্যে পরিবর্তন করা। প্রোগ্রামাররা তাদের সিস্টেমে ইনপুট তুলনা করার মত কাজে ধারাবাহিকতা আনার জন্য এটা করে থাকেন।

## কিভাবে:

```ruby
# downcase মেথড ব্যবহার করে
my_string = "Hello World!"
puts my_string.downcase  # => "hello world!"
```

```ruby
# সরাসরি পরিবর্তনের জন্য downcase! ব্যবহার করে
my_string = "Hello World!"
my_string.downcase!
puts my_string           # => "hello world!"
```

## গভীর ডাইভ

ঐতিহাসিকভাবে, কেস রূপান্তরণ প্রোগ্রামিং ভাষাগুলোতে টেক্সটের ঐক্যমত্য নিশ্চিত করতে একটি মৌলিক বিষয় রূপে বিবেচিত হয়ে থাকে। এটি কেস-অনভিজ্ঞ তুলনা এবং অনুসন্ধানে সহায়ক, তাই এর গুরুত্ব রয়েছে।

রুবির `downcase` এবং `downcase!` মেথডগুলি অ-ধ্বংসাত্মক এবং ধ্বংসাত্মক উভয় প্রকারের স্ট্রিং ম্যানিপুলেশন মেথড প্রদানের ভাষার নীতি থেকে উদ্ভূত হয়েছে। অ-ধ্বংসাত্মক `downcase` একটি নতুন স্ট্রিং ফিরিয়ে দেয়, মূলটি অবিকৃত রাখে, এবং ধ্বংসাত্মক `downcase!` মূল স্ট্রিংটিকে স্থানে পরিবর্তন করে, যা মেমরির দক্ষতা বৃদ্ধি করতে পারে।

যখন লোকেল-নির্ধারিত নীতি প্রযোজ্য হয়, তখন বিকল্প পদ্ধতি রয়েছে। অ্যাকসেন্ট বা অন্যান্য দিয়াক্রিটিকাল চিহ্নযুক্ত অক্ষরের মত জটিল পরিস্থিতি সামাল দিতে `String#mb_chars` সম্বন্ধে `ActiveSupport::Multibyte::Chars#downcase` ব্যবহার করা যেতে পারে:
```ruby
require 'active_support/core_ext/string/multibyte'

my_string = "ÄÖÜ"
puts my_string.mb_chars.downcase  # => "äöü"
```

বাস্তবায়নের ক্ষেত্রে, রুবির `downcase` এবং `downcase!` প্রতিটি অক্ষরকে তার ছোট হাতের সমতুল্যে পরিবর্তন করতে ইউনিকোড ম্যাপিং ব্যবহার করে।

## আরো দেখুন

- `downcase` এবং `downcase!` এর জন্য রুবি ডকুমেন্টেশন: [Ruby Doc downcase](https://ruby-doc.org/core-3.1.2/String.html#method-i-downcase), [Ruby Doc downcase!](https://ruby-doc.org/core-3.1.2/String.html#method-i-downcase-21)
- জটিল কেস রূপান্তরের জন্য, দেখুন অ্যাক্টিভসাপোর্ট কোর এক্সটেনশনস: [ActiveSupport String](https://api.rubyonrails.org/classes/String.html)

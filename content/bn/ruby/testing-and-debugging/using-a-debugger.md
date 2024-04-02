---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:22:36.098744-06:00
description: "\u09B0\u09C1\u09AC\u09BF\u09B0 \u09B8\u09BE\u09A5\u09C7 \u098F\u0995\
  \u099F\u09BF \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u09A1\u09BF\u09AC\u09BE\
  \u0997\u09BE\u09B0 \u0986\u09B8\u09C7 \u09AF\u09BE\u09B0 \u09A8\u09BE\u09AE `byebug`\u0964\
  \ \u09AA\u09CD\u09B0\u09A5\u09AE\u09C7, \u0986\u09AA\u09A8\u09BE\u09B0 Gemfile-\u098F\
  \ `byebug` \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09AD\u09C1\u0995\u09CD\u09A4 \u0995\
  \u09B0\u09C1\u09A8 \u098F\u09AC\u0982 `bundle install` \u099A\u09BE\u09B2\u09BE\u09A8\
  \u0964 \u09A4\u09BE\u09B0\u09AA\u09B0, \u0986\u09AA\u09A8\u09BE\u09B0\u2026"
lastmod: '2024-03-17T18:47:44.594995-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09C1\u09AC\u09BF\u09B0 \u09B8\u09BE\u09A5\u09C7 \u098F\u0995\u099F\
  \u09BF \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u09A1\u09BF\u09AC\u09BE\u0997\
  \u09BE\u09B0 \u0986\u09B8\u09C7 \u09AF\u09BE\u09B0 \u09A8\u09BE\u09AE `byebug`\u0964\
  \ \u09AA\u09CD\u09B0\u09A5\u09AE\u09C7, \u0986\u09AA\u09A8\u09BE\u09B0 Gemfile-\u098F\
  \ `byebug` \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09AD\u09C1\u0995\u09CD\u09A4 \u0995\
  \u09B0\u09C1\u09A8 \u098F\u09AC\u0982 `bundle install` \u099A\u09BE\u09B2\u09BE\u09A8\
  \u0964 \u09A4\u09BE\u09B0\u09AA\u09B0, \u0986\u09AA\u09A8\u09BE\u09B0\u2026"
title: "\u09A1\u09BF\u09AC\u09BE\u0997\u09BE\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09BE"
weight: 35
---

## কিভাবে:
রুবির সাথে একটি বিল্ট-ইন ডিবাগার আসে যার নাম `byebug`। প্রথমে, আপনার Gemfile-এ `byebug` অন্তর্ভুক্ত করুন এবং `bundle install` চালান। তারপর, আপনার প্রোগ্রামটি যেখানে বিরতি নিতে চান সেখানে `byebug` স্থাপন করুন।

```Ruby
require 'byebug'

def calculate_magic(number)
  byebug
  magic_number = number * 7
  return magic_number
end

puts calculate_magic(6)
```

এই স্ক্রিপ্টটি চালানো হলে, `byebug`-এ কার্যক্রম স্থগিত হবে, এবং আপনি একটি ইন্টার‌্যাক্টিভ সেশনে প্রবেশ করবেন যেখানে আপনি যেমন কমান্ড টাইপ করতে পারেন:

```
step
next
continue
var local
```

নমুনা আউটপুট এইরূপ একটি প্রম্প্ট দেবে:

```
[2, 11] in example.rb
    2: 
    3: def calculate_magic(number)
    4:   byebug
=>  5:   magic_number = number * 7
    6:   return magic_number
    7: end
    8: 
    9: puts calculate_magic(6)
(byebug) 
```

## গভীরে ডুব:
`byebug` এর আগে, রুবি ডেভেলপাররা `debugger` এবং `pry` ব্যবহার করতো। `pry` শুধু একটি ডিবাগার নয়, এটি একটি শক্তিশালী REPL যা `binding.pry` ব্রেকপয়েন্ট দিয়ে ডিবাগিংয়ের জন্যও ব্যবহার করা যায়।

রুবির `byebug` এর বিকল্পগুলো অন্তর্ভুক্ত করে `pry-byebug`, যা `pry` এর সাথে `byebug` ফাংশনালিটি সমন্বিত করে, এবং `ruby-debug`, যা একটি পুরানো জেম যা সক্রিয়ভাবে রক্ষণাবেক্ষণ করা হয় না।

আপনি যখন `byebug` আহ্বান করেন, ডিবাগারটি আপনার কোড কার্যকরণ স্থগিত করে এবং রানটাইমে একটি ঝলক দেখার সুযোগ দেয়। আপনি ভেরিয়েবল দেখতে এবং পরিবর্তন করতে, কোডের ভিন্ন ভিন্ন স্থানে লাফিয়ে যেতে, এবং এমনকি কিছু রুবি কোড লাইন বাই লাইন চালাতে পারেন। এটি আপনার রুবি কোডের জন্য সময় ভ্রমণের মতো ক্ষমতা হতে পারে।

## আরও দেখুন:
- Byebug GitHub Repository: [https://github.com/deivid-rodriguez/byebug](https://github.com/deivid-rodriguez/byebug)
- Pry ডকুমেন্টেশন: [https://github.com/pry/pry](https://github.com/pry/pry)
- রেলস অ্যাপস ডিবাগ করার গাইড: [https://guides.rubyonrails.org/debugging_rails_applications.html](https://guides.rubyonrails.org/debugging_rails_applications.html)

---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:36.897961-06:00
description: "\u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982\u09AF\
  \u09BC\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \ \u0995\u09CD\u09AF\u09BE\u09AA\u09BF\u099F\u09BE\u09B2\u09BE\u0987\u099C \u0995\
  \u09B0\u09BE \u09AA\u09CD\u09B0\u09BE\u09AF\u09BC\u0987 \u09B8\u09CD\u099F\u09CD\
  \u09B0\u09BF\u0982\u09AF\u09BC\u09C7\u09B0 \u09AA\u09CD\u09B0\u09A5\u09AE \u0985\
  \u0995\u09CD\u09B7\u09B0\u099F\u09BF \u09AC\u09A1\u09BC \u09B9\u09BE\u09A4\u09C7\
  \u09B0 \u0985\u0995\u09CD\u09B7\u09B0\u09C7 (uppercase) \u09B0\u09C2\u09AA\u09BE\
  \u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE \u098F\u09AC\u0982 \u09AC\u09BE\u0995\
  \u09BF\u0997\u09C1\u09B2\u09CB\u0995\u09C7 \u099B\u09CB\u099F \u09B9\u09BE\u09A4\
  \u09C7\u09B0 \u0985\u0995\u09CD\u09B7\u09B0\u09C7\u2026"
lastmod: '2024-03-17T18:47:44.569798-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982\u09AF\
  \u09BC\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \ \u0995\u09CD\u09AF\u09BE\u09AA\u09BF\u099F\u09BE\u09B2\u09BE\u0987\u099C \u0995\
  \u09B0\u09BE \u09AA\u09CD\u09B0\u09BE\u09AF\u09BC\u0987 \u09B8\u09CD\u099F\u09CD\
  \u09B0\u09BF\u0982\u09AF\u09BC\u09C7\u09B0 \u09AA\u09CD\u09B0\u09A5\u09AE \u0985\
  \u0995\u09CD\u09B7\u09B0\u099F\u09BF \u09AC\u09A1\u09BC \u09B9\u09BE\u09A4\u09C7\
  \u09B0 \u0985\u0995\u09CD\u09B7\u09B0\u09C7 (uppercase) \u09B0\u09C2\u09AA\u09BE\
  \u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE \u098F\u09AC\u0982 \u09AC\u09BE\u0995\
  \u09BF\u0997\u09C1\u09B2\u09CB\u0995\u09C7 \u099B\u09CB\u099F \u09B9\u09BE\u09A4\
  \u09C7\u09B0 \u0985\u0995\u09CD\u09B7\u09B0\u09C7\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09AA\u09CD\u09B0\
  \u09A5\u09AE \u0985\u0995\u09CD\u09B7\u09B0 \u09AC\u09A1\u09BC \u09B9\u09BE\u09A4\
  \u09C7\u09B0 \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কী এবং কেন?
প্রোগ্রামিংয়ে একটি স্ট্রিং ক্যাপিটালাইজ করা প্রায়ই স্ট্রিংয়ের প্রথম অক্ষরটি বড় হাতের অক্ষরে (uppercase) রূপান্তর করা এবং বাকিগুলোকে ছোট হাতের অক্ষরে (lowercase) রূপান্তর করাকে বোঝায়। নামকরণের নিয়ম মেনে চলা, আউটপুটগুলি আরও পড়াযোগ্য করে তোলা, অথবা তুলনা এবং সংরক্ষণের জন্য ডেটা সামঞ্জস্য নিশ্চিত করার মত কারণে প্রোগ্রামাররা এটি করে থাকেন।

## কিভাবে:
রুবি স্ট্রিং ম্যানিপুলেশনের জন্য সরল পদ্ধতি প্রদান করে, তার মধ্যে ক্যাপিটালাইজেশন একটি। রুবিতে একটি স্ট্রিং ক্যাপিটালাইজ করে তুলতে আপনি কিভাবে করবেন তা এখানে দেখানো হল:

```ruby
# রুবির বিল্ট-ইন পদ্ধতি
string = "hello world"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

রুবির `.capitalize` পদ্ধতি সুবিধাজনক কিন্তু শুধুমাত্র প্রথম অক্ষরটি প্রভাবিত করে। প্রতিটি শব্দ ক্যাপিটালাইজ করার জন্য (যাকে টাইটেল কেস বলা হয়) আপনি হয়তো Rails ActiveSupport এক্সটেনশনের `titleize` পদ্ধতিটি ব্যবহার করতে চাইবেন, অথবা নিজে এটি ইমপ্লিমেন্ট করতে চাইবেন:

```ruby
# রেইলসে ActiveSupport-র 'titleize' ব্যবহার করা
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

যদি আপনি রেইলস ব্যবহার না করেন অথবা একটি শুদ্ধ রুবি সমাধান প্রাধান্য দেন, তাহলে একটি স্ট্রিংয়ের প্রতিটি শব্দ ক্যাপিটালাইজ করার উপায় এখানে দেখানো হল:

```ruby
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

এই পদ্ধতিটি স্ট্রিংটিকে শব্দের একটি অ্যারেতে বিভক্ত করে, প্রতিটি শব্দকে ক্যাপিটালাইজ করে, তারপর তাদেরকে একটি স্পেস দিয়ে আবার একত্রিত করে।

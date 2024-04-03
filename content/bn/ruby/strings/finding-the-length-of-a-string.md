---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:42.163652-06:00
description: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09AF\
  \u09BC\u09C7\u09B0 \u09A6\u09C8\u09B0\u09CD\u0998\u09CD\u09AF \u09A8\u09BF\u09B0\
  \u09CD\u09A3\u09AF\u09BC \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u09B0\
  \ \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u0997\u09A3\u09A8\u09BE\
  \ \u0995\u09B0\u09BE\u0964 \u098F\u099F\u09BF \u09AA\u09CD\u09B0\u09BE\u09A5\u09AE\
  \u09BF\u0995 \u0995\u09BF\u09A8\u09CD\u09A4\u09C1 \u09AF\u09BE\u099A\u09BE\u0987\
  \u0995\u09B0\u09A3, \u099F\u09C7\u0995\u09CD\u09B8\u099F-\u09AA\u09CD\u09B0\u0995\
  \u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u0995\u09B0\u09A3 \u098F\u09AC\u0982 \u09B8\
  \u099E\u09CD\u099A\u09AF\u09BC\u09C7\u09B0 \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\
  \u09A8 \u09A8\u09BF\u09B0\u09CD\u09A7\u09BE\u09B0\u09A3\u09C7\u09B0 \u09AE\u09A4\
  \u09CB\u2026"
lastmod: '2024-03-17T18:47:44.578084-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09AF\
  \u09BC\u09C7\u09B0 \u09A6\u09C8\u09B0\u09CD\u0998\u09CD\u09AF \u09A8\u09BF\u09B0\
  \u09CD\u09A3\u09AF\u09BC \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u09B0\
  \ \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u0997\u09A3\u09A8\u09BE\
  \ \u0995\u09B0\u09BE\u0964 \u098F\u099F\u09BF \u09AA\u09CD\u09B0\u09BE\u09A5\u09AE\
  \u09BF\u0995 \u0995\u09BF\u09A8\u09CD\u09A4\u09C1 \u09AF\u09BE\u099A\u09BE\u0987\
  \u0995\u09B0\u09A3, \u099F\u09C7\u0995\u09CD\u09B8\u099F-\u09AA\u09CD\u09B0\u0995\
  \u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u0995\u09B0\u09A3 \u098F\u09AC\u0982 \u09B8\
  \u099E\u09CD\u099A\u09AF\u09BC\u09C7\u09B0 \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\
  \u09A8 \u09A8\u09BF\u09B0\u09CD\u09A7\u09BE\u09B0\u09A3\u09C7\u09B0 \u09AE\u09A4\
  \u09CB \u0995\u09BE\u099C\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u0985\u09A4\u09CD\
  \u09AF\u09BE\u09AC\u09B6\u09CD\u09AF\u0995\u0964."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09A6\u09C8\u09B0\
  \u09CD\u0998\u09CD\u09AF \u099A\u09BF\u09B9\u09CD\u09A8\u09BF\u09A4 \u0995\u09B0\
  \u09BE"
weight: 7
---

## কি এবং কেন?
একটি স্ট্রিংয়ের দৈর্ঘ্য নির্ণয় করা মানে এর অক্ষরগুলি গণনা করা। এটি প্রাথমিক কিন্তু যাচাইকরণ, টেক্সট-প্রক্রিয়াকরণ এবং সঞ্চয়ের প্রয়োজন নির্ধারণের মতো কাজের জন্য অত্যাবশ্যক।

## কিভাবে:
রুবির `.length` মেথডের সাথে এটি সহজ:

```ruby
greeting = "Hello, world!"
puts greeting.length
```

আউটপুট:

```
13
```

অথবা, `.size` ব্যবহার করুন যা একই কাজ করে:

```ruby
greeting = "Hello, world!"
puts greeting.size
```

আউটপুট:

```
13
```

## গভীর ডাইভ
রুবিতে, যখন স্ট্রিং এর কথা আসে তখন `.length` এবং `.size` পরিবর্তনযোগ্য; উভয়ই আপনাকে অক্ষরের সংখ্যা দেয়। ঐতিহাসিকভাবে, রুবি কোডকে আরও 
স্বাভাবিকভাবে পড়ার জন্য মনোনিবেশ করেছে, যার ফলে আপনি প্রায়ই একই জিনিস করার জন্য একাধিক উপায় পাবেন।

অভ্যন্তরীণভাবে, একটি স্ট্রিং-এ প্রতিটি অক্ষর স্টোরেজ আকারকে প্রভাবিত করে। তাই, বিশেষ করে প্রচুর পরিমাণে টেক্সটের সাথে সংখ্যাটি জানা অপরিহার্য হতে পারে।

`.length` এবং `.size` যেখানে অক্ষরের সংখ্যা দেয়, কিছু ভাষা এবং পূর্বের সময়ে, একটি স্ট্রিং-এর দৈর্ঘ্য এটির বাইট আকারের প্রসঙ্গে
থাকতে পারে। তবে ইউনিকোডের মাধ্যমে মাল্টিবাইট অক্ষরের সমর্থনের মাধ্যমে, রুবি, অক্ষর একাধিক বাইট নেয়ার কারণে সরাসরি বাইট আকারকে 
স্ট্রিংয়ের দৈর্ঘ্যের সাথে সমতা করে না।

`.bytesize` মতো বিকল্পগুলি আপনাকে জানায় একটি স্ট্রিং কত বাইট দখল করে, এবং `.chars.count` প্রথমে স্ট্রিংটিকে অক্ষরের একটি অ্যারেতে রূপান্তর করে অনুমান করার মাধ্যমে অক্ষরের সংখ্যা দেয়।

`.bytesize` এবং `.chars.count` কিভাবে ব্যবহার করা যায় দেখুন:

```ruby
greeting = "Hello, world!"
puts greeting.bytesize
puts greeting.chars.count
```

আউটপুট:

```
13
13
```

## আরও দেখুন
- রুবি ডকুমেন্টেশন স্ট্রিংস সম্পর্কে: [https://ruby-doc.org/core/String.html](https://ruby-doc.org/core/String.html)
- রুবি স্ট্রিংস দ্বারা একটি সুন্দর প্রাথমিক ভূমিকা [RubyGuides](https://www.rubyguides.com/2018/01/ruby-string-methods/)-এ খোঁজুন: স্ট্রিংয়ের আকার পরিমাপ ছাড়াও আপনি স্ট্রিংয়ের সাথে কি কি করতে পারেন তা আরও অন্বেষণ করুন।
- অক্ষরের এনকোডিং এবং এটি স্ট্রিং অপারেশনগুলিকে কিভাবে প্রভাবিত করে সে সম্পর্কে ডুব দিন [Thoughtbot](https://thoughtbot.com/blog/its-about-time-zones#character-encoding)-এর এই নিবন্ধে।

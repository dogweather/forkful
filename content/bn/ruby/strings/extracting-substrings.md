---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:00.186990-06:00
description: "\u0989\u09AA\u09B8\u09CD\u09A5\u09BF\u09A4\u09BF \u09AC\u09BE\u099B\u09BE\
  \u0987 \u0995\u09B0\u09BE \u0985\u09B0\u09CD\u09A5\u09BE\u09CE \u09AC\u09BF\u09B6\
  \u09C7\u09B7 \u09AC\u09BF\u099F \u099F\u09C7\u0995\u09CD\u09B8\u099F \u098F\u0995\
  \u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7\
  \ \u09AA\u09CD\u09B0\u09A4\u09CD\u09AF\u09BE\u09B9\u09BE\u09B0\u0964 \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF\
  \ \u0995\u09B0\u09C7 \u09A1\u09C7\u099F\u09BE\u09B0 \u0985\u0982\u09B6 \u09AC\u09BF\
  \u09B6\u09C7\u09B7 \u09A8\u09BF\u09AF\u09BC\u09A8\u09CD\u09A4\u09CD\u09B0\u09A3\
  \ \u0993 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u09C7\u09B0 \u099C\u09A8\u09CD\
  \u09AF\u2014\u09AF\u09C7\u09AE\u09A8 \u0987\u09AE\u09C7\u09B2\u2026"
lastmod: '2024-03-17T18:47:44.575798-06:00'
model: gpt-4-0125-preview
summary: "\u0989\u09AA\u09B8\u09CD\u09A5\u09BF\u09A4\u09BF \u09AC\u09BE\u099B\u09BE\
  \u0987 \u0995\u09B0\u09BE \u0985\u09B0\u09CD\u09A5\u09BE\u09CE \u09AC\u09BF\u09B6\
  \u09C7\u09B7 \u09AC\u09BF\u099F \u099F\u09C7\u0995\u09CD\u09B8\u099F \u098F\u0995\
  \u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7\
  \ \u09AA\u09CD\u09B0\u09A4\u09CD\u09AF\u09BE\u09B9\u09BE\u09B0\u0964 \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF\
  \ \u0995\u09B0\u09C7 \u09A1\u09C7\u099F\u09BE\u09B0 \u0985\u0982\u09B6 \u09AC\u09BF\
  \u09B6\u09C7\u09B7 \u09A8\u09BF\u09AF\u09BC\u09A8\u09CD\u09A4\u09CD\u09B0\u09A3\
  \ \u0993 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u09C7\u09B0 \u099C\u09A8\u09CD\
  \u09AF\u2014\u09AF\u09C7\u09AE\u09A8 \u0987\u09AE\u09C7\u09B2 \u09A0\u09BF\u0995\
  \u09BE\u09A8\u09BE \u09A5\u09C7\u0995\u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0\u0995\u09BE\u09B0\u09C0\u09B0 \u09A8\u09BE\u09AE \u09AC\u09BE \u099F\u09BE\
  \u0987\u09AE\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09AE\u09CD\u09AA \u09A5\u09C7\u0995\
  \u09C7 \u09A4\u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\
  \u09BE\u0964."
title: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AC\u09C7\u09B0\
  \ \u0995\u09B0\u09BE"
weight: 6
---

## কিভাবে:
রুবি উপস্থিতি বাছাই করা সহজ করে দেয়। চলুন সরাসরি মূল প্রসঙ্গে চলে যাই:

```Ruby
str = "Hello, Ruby World!"

# পদ্ধতি ১: অ্যারে ইনডেক্স ব্যবহার
substring = str[7, 4] # "Ruby"
puts substring

# পদ্ধতি ২: স্লাইস মেথড ব্যবহার
slice = str.slice(7, 4) # "Ruby"
puts slice

# পদ্ধতি ৩: নিয়মিত এক্সপ্রেশন
match = str[/[Rr]uby/] # "Ruby"
puts match

# পদ্ধতি ৪: স্প্লিট এবং অ্যারে অ্যাক্সেস
split_array = str.split # ডিফল্ট শ্বেতস্থানে ভাগে
picked_word = split_array[2] # "World!"
puts picked_word
```

প্রতি টুকরোর জন্য নমুনা আউটপুট যথাক্রমে "Ruby", "Ruby", "Ruby", "World!" হবে।

## গভীর ডুব
প্রাচীন সময়ে, উপস্থিতি বাছাই করা আরও বহুবাক্যালাপিক প্রক্রিয়া ছিল। তবে রুবি উন্নত হয়েছে। আজ, আপনার কাছে মেথড এবং রেগেক্স রয়েছে।

এখানে যা ঘটছে তা হলঃ `[7, 4]` মানে সপ্তম অক্ষরে শুরু করে পরবর্তী ৪টি নেওয়া। `slice` হল একই জিনিসকে পদ্ধতিগতভাবে বলা। রেগেক্স সাহায্যে, `/[Rr]uby/` মানে "প্রথমে যা পাও, 'Ruby' বা 'ruby’ ধরে ফেল।" `split` প্রতি স্থানে স্ট্রিংটি একটি অ্যারেতে ভাগ করে, এবং `[2]` তৃতীয় শব্দটি নির্বাচন করে—অ্যারে শূন্য থেকে শুরু হয়, মনে রাখবেন।

বিকল্প? অবশ্যই, রুবির তাদের আছে। `partition`, `rpartition`, এবং `match` এখানেও খেলা করতে পারে। প্রতিটির নিজস্ব কেস রয়েছে কিন্তু `.slice` এবং রেগেক্স অধিকাংশ বেস কভার করে।

মূল কথা: উপস্থিতি বাছাই সম্পর্কে নির্দিষ্ট টেক্সট নিয়ন্ত্রণের বিষয়। সঠিক টুল মানে পরিষ্কার, কার্যকর কোড।

## আরও দেখুন
- রুবি ডকস অন স্ট্রিং: [ruby-doc.org/core-2.7.0/String.html](https://ruby-doc.org/core-2.7.0/String.html)
- রুবিতে নিয়মিত এক্সপ্রেশন: [ruby-doc.org/core-2.7.0/Regexp.html](https://ruby-doc.org/core-2.7.0/Regexp.html)
- স্ট্রিংস অনুসারে রুবি স্টাইল গাইড: [rubystyle.guide/#strings](https://rubystyle.guide/#strings)

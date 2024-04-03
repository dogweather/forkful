---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:50:38.241099-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09A1\u09BE\u09B0\u09CD\u099F\
  \u09C7, \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\
  \u09B0\u09AA\u09CB\u09B2\u09C7\u09B6\u09A8 \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF\
  , `$` \u09AA\u09CD\u09B0\u09A4\u09C0\u0995 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09B2\u09BF\u099F\
  \u09BE\u09B0\u09BE\u09B2\u09C7\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u09AA\u09CD\
  \u09B0\u0995\u09BE\u09B6\u09A8\u0997\u09C1\u09B2\u09BF \u0987\u09A8\u09CD\u099F\u09BE\
  \u09B0\u09AA\u09CB\u09B2\u09C7\u099F \u0995\u09B0\u09BE\u09B0 \u09AE\u09BE\u09A7\
  \u09CD\u09AF\u09AE\u09C7 \u09B8\u09B9\u099C."
lastmod: '2024-03-17T18:47:43.701955-06:00'
model: gpt-4-0125-preview
summary: "\u09A1\u09BE\u09B0\u09CD\u099F\u09C7, \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AA\u09CB\u09B2\u09C7\u09B6\u09A8\
  \ \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF, `$` \u09AA\u09CD\u09B0\u09A4\u09C0\u0995\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09B8\u09CD\u099F\
  \u09CD\u09B0\u09BF\u0982 \u09B2\u09BF\u099F\u09BE\u09B0\u09BE\u09B2\u09C7\u09B0\
  \ \u09AE\u09A7\u09CD\u09AF\u09C7 \u09AA\u09CD\u09B0\u0995\u09BE\u09B6\u09A8\u0997\
  \u09C1\u09B2\u09BF \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AA\u09CB\u09B2\u09C7\u099F\
  \ \u0995\u09B0\u09BE\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09B8\u09B9\
  \u099C."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\
  \u09AA\u09CB\u09B2\u09C7\u099F \u0995\u09B0\u09BE"
weight: 8
---

## কিভাবে:
ডার্টে, স্ট্রিং ইন্টারপোলেশন সরাসরি, `$` প্রতীক ব্যবহার করে স্ট্রিং লিটারালের মধ্যে প্রকাশনগুলি ইন্টারপোলেট করার মাধ্যমে সহজ:

```dart
void main() {
  String name = 'ডার্ট';
  int year = 2023;
  // সহজ পরিবর্তনশীল ইন্টারপোলেশন
  print('শিখছি $name $year সালে!');
  // আউটপুট: শিখছি ডার্ট 2023 সালে!
  
  // প্রকাশনগুলি ইন্টারপোলেট করা
  print('দুই বছরে, এটি হবে ${year + 2}.');
  // আউটপুট: দুই বছরে, এটি হবে 2025.
}
```

যেখানে আপনার আরও জটিল প্রকাশন থাকে বা স্ট্রিংয়ের মধ্যে নিজেই অপারেশন সম্পাদন করতে চান, সেক্ষেত্রে প্রকাশনটি `${}` দিয়ে বন্ধ করুন। ডার্টের স্ট্রিং ইন্টারপোলেশনের জন্য কোনো জনপ্রিয় থার্ড-পার্টি লাইব্রেরি নেই কারণ এটি নিজে থেকেই বিভিন্ন এবং জটিল পরিস্থিতিগুলি সামলানোর জন্য ভালভাবে সজ্জিত।

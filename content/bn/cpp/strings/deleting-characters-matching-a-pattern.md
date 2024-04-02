---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:14.273872-06:00
description: "\u098F\u0995\u099F\u09BF \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\
  \u099F \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8\u09C7\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u09AE\u09C7\u09B2\u09C7 \u098F\u09AE\u09A8 \u0985\u0995\u09CD\
  \u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\
  \u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\
  \u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\
  \u09B7\u09CD\u099F \u09B8\u09BF\u0995\u09CB\u09AF\u09BC\u09C7\u09A8\u09CD\u09B8\u0997\
  \u09C1\u09B2\u09BF \u0985\u09AA\u09B8\u09BE\u09B0\u09A3 \u0995\u09B0\u09BE\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u098F\u099F\u09BE \u09AA\u09B0\u09BF\u09B7\u09CD\u0995\u09BE\u09B0 \u0995\u09B0\
  \u09BE\u09B0\u2026"
lastmod: '2024-03-17T18:47:44.348447-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\
  \u099F \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8\u09C7\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u09AE\u09C7\u09B2\u09C7 \u098F\u09AE\u09A8 \u0985\u0995\u09CD\
  \u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\
  \u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\
  \u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\
  \u09B7\u09CD\u099F \u09B8\u09BF\u0995\u09CB\u09AF\u09BC\u09C7\u09A8\u09CD\u09B8\u0997\
  \u09C1\u09B2\u09BF \u0985\u09AA\u09B8\u09BE\u09B0\u09A3 \u0995\u09B0\u09BE\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u098F\u099F\u09BE \u09AA\u09B0\u09BF\u09B7\u09CD\u0995\u09BE\u09B0 \u0995\u09B0\
  \u09BE\u09B0\u2026"
title: "\u098F\u0995\u099F\u09BF \u09A8\u09AE\u09C1\u09A8\u09BE \u09AE\u09C7\u09B2\
  \u09C7 \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09AE\u09C1\u099B\
  \u09C7 \u09AB\u09C7\u09B2\u09BE"
weight: 5
---

## কি এবং কেন?
একটি নির্দিষ্ট প্যাটার্নের সাথে মেলে এমন অক্ষরগুলি মুছে ফেলা মানে একটি স্ট্রিং থেকে নির্দিষ্ট সিকোয়েন্সগুলি অপসারণ করা। প্রোগ্রামাররা এটা পরিষ্কার করার জন্য, ডেটা ফরম্যাটিং অথবা অ্যাপ্লিকেশনের নিয়মাবলী পূরণের জন্য করে থাকেন।

## কিভাবে:
আসুন `erase` এবং `remove_if`-এর সাথে ল্যাম্বডা এক্সপ্রেশনগুলি ব্যবহার করে অক্ষরগুলি মুছে ফেলি। এখানে একটি দ্রুত উদাহরণ দেওয়া হল:

```cpp
#include <iostream>
#include <algorithm>
#include <string>

int main() {
    std::string data = "B4n4n4!";

    // সমস্ত সংখ্যা সংকেত অপসারণ
    data.erase(std::remove_if(data.begin(), data.end(), ::isdigit), data.end());
    
    std::cout << data << std::endl; // আউটপুট: Bnn!
    
    return 0;
}
```
নমুনা আউটপুট:
```
Bnn!
```

## গভীর ডুব
`<algorithm>` হেডার থেকে `std::remove_if` অ্যালগরিদমটি আসলে স্ট্রিংটিকে ছোট করে না; এটি উপাদানগুলিকে পুনর্বিন্যাস করে এবং নতুন যৌক্তিক শেষে একটি নির্দেশক ফেরত দেয়। তারপরে `std::string` ক্লাসের `erase` পদ্ধতি শেষ থেকে "মৃত কাঠ" অপসারণ করে। এই কম্বো C++98 এর সাথে এসেছে এবং এফিসিয়েন্ট এবং জনপ্রিয় রয়ে গেছে।

বিকল্প? জটিল প্যাটার্নের জন্য, regex (`<regex>`) হল আপনার সুইস আর্মি ছুরি। তবে, সাধারণ কাজের জন্য এটি বেশি।

বিস্তারিত? `std::remove_if` এবং এই ধরনের অ্যালগরিদমগুলি ইটারেটরগুলির ওপর নির্ভর করে, যা C++ মাঝ-৯০ এর দশকে স্ট্যান্ডার্ড টেমপ্লেট লাইব্রেরি (STL) থেকে গ্রহণ করেছিল। তারা জেনেরিক প্রোগ্রামিংকে শক্তি দেয়, নিশ্চিত করে যে আপনার কাটা-ছেঁটা কোড স্ট্রিং, লিস্ট, ইত্যাদি উপর কাজ করে।

## আরও দেখুন
- `std::remove_if`-এর জন্য C++ রেফারেন্স: https://en.cppreference.com/w/cpp/algorithm/remove
- `std::string::erase`-এর জন্য C++ রেফারেন্স: https://en.cppreference.com/w/cpp/string/basic_string/erase
- C++ এ ইটারেটরগুলি সম্পর্কে আরও জানুন: https://en.cppreference.com/w/cpp/iterator
- প্যাটার্ন মিলানোর জন্য `std::regex` কখন ব্যবহার করতে হবে: https://en.cppreference.com/w/cpp/regex

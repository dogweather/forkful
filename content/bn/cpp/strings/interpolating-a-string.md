---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:50:53.173481-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: C++-\u098F \u0985\u09A8\u09CD\u09AF\
  \u09BE\u09A8\u09CD\u09AF \u09AD\u09BE\u09B7\u09BE\u09B0 \u09AE\u09A4\u09CB \u09AC\
  \u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \ \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AA\u09CB\u09B2\u09C7\u09B6\u09A8 \u09A8\
  \u09C7\u0987\u0964 \u09AA\u09CD\u09B0\u09BE\u09AF\u09BC\u09B6\u0987 \u0986\u09AA\
  \u09A8\u09BF `std::ostringstream`, `std::format` (C++20 \u09A5\u09C7\u0995\u09C7\
  ), \u0985\u09A5\u09AC\u09BE printf-\u09B6\u09C8\u09B2\u09C0\u09B0\u2026"
lastmod: '2024-03-17T18:47:44.350501-06:00'
model: gpt-4-0125-preview
summary: "C++-\u098F \u0985\u09A8\u09CD\u09AF\u09BE\u09A8\u09CD\u09AF \u09AD\u09BE\
  \u09B7\u09BE\u09B0 \u09AE\u09A4\u09CB \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\
  \u09AA\u09CB\u09B2\u09C7\u09B6\u09A8 \u09A8\u09C7\u0987\u0964 \u09AA\u09CD\u09B0\
  \u09BE\u09AF\u09BC\u09B6\u0987 \u0986\u09AA\u09A8\u09BF `std::ostringstream`, `std::format`\
  \ (C++20 \u09A5\u09C7\u0995\u09C7), \u0985\u09A5\u09AC\u09BE printf-\u09B6\u09C8\
  \u09B2\u09C0\u09B0 \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F\u09BF\u0982 \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\
  \u09A8\u0964\n\n`std::ostringstream` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09C7."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\
  \u09AA\u09CB\u09B2\u09C7\u099F \u0995\u09B0\u09BE"
weight: 8
---

## কিভাবে:
C++-এ অন্যান্য ভাষার মতো বিল্ট-ইন স্ট্রিং ইন্টারপোলেশন নেই। প্রায়শই আপনি `std::ostringstream`, `std::format` (C++20 থেকে), অথবা printf-শৈলীর ফরম্যাটিং ব্যবহার করে থাকেন।

`std::ostringstream` ব্যবহার করে:
```cpp
#include <sstream>
#include <iostream>

int main() {
    std::ostringstream message;
    int age = 30;
    message << "Hello, I am " << age << " years old.";
    std::cout << message.str() << std::endl; // "Hello, I am 30 years old."
}
```

`std::format` ব্যবহার করে (C++20):
```cpp
#include <format>
#include <iostream>

int main() {
    int age = 30;
    std::string message = std::format("Hello, I am {} years old.", age);
    std::cout << message << std::endl; // "Hello, I am 30 years old."
}
```

## গভীরে প্রবেশ
C++20 এর আগে, আমরা স্ট্রিংগুলি স্ট্রিম বা sprintf এর সাথে জোড়ালাম, যা ছিল ঝামেলাপূর্ণ। `std::format`-এর আবির্ভাবের সাথে সাথে, আমরা পাইথনের f-স্ট্রিংগুলির মতো আধুনিক ভাষার সাথে সম্পৃক্ত হচ্ছি।

`std::ostringstream`: এটি আমাদের স্ট্রিং তৈরির উপায় হিসেবে একটি স্ট্রিমের মতো পদ্ধতি দেয়। এটি বহুমুখী কিন্তু সবচেয়ে সংক্ষিপ্ত নয়। এটি বহু বছর ধরে নিরাপদ এবং ব্যবহার করা সহজ বলে প্রধান পছন্দ হয়ে উঠেছে।

`std::format`: C++20-এ চালু হয়েছে, এটি পাইথনের মতো ফর্ম্যাটিং অফার করে। এটি স্ট্রিম যোগফলের তুলনায় আরও পঠনযোগ্য এবং দক্ষ, তবে আরও নতুন কম্পাইলার প্রয়োজন।

Boost.Format বা স্ট্রিং যোগফল ব্যবহারের মতো বিকল্প বিদ্যমান, কিন্তু তারা এতটা পরিষ্কার না অথবা অতিরিক্ত খরচ বহন করতে পারে।

স্ট্রিং ইন্টারপোলেশন হল চিনির মতো, তবে এটি মিষ্টি। এটি কোডকে সহজীকরণ করে এবং বারবার স্ট্রিং যোগ করার কারণে পারফরম্যান্স হ্রাস এড়ায়।

## আরও দেখুন
- [cppreference এ std::format](https://en.cppreference.com/w/cpp/utility/format)
- [cppreference এ std::ostringstream](https://en.cppreference.com/w/cpp/io/basic_ostringstream)
- [Boost.Format লাইব্রেরি](https://www.boost.org/doc/libs/release/libs/format/)

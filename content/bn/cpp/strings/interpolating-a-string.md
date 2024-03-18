---
title:                "স্ট্রিং ইন্টারপোলেট করা"
date:                  2024-03-17T17:50:53.173481-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
স্ট্রিং ইন্টারপোলেশন হচ্ছে স্ট্রিং-এর মধ্যে ভেরিয়েবল ঢোকানো। আমরা এটি করি মেসেজ স্বয়ংক্রিয়ভাবে তৈরি, আউটপুট ব্যক্তিগতকরণ অথবা ডাইনামিক কোয়েরী তৈরির জন্য।

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

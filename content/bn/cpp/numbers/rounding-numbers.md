---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:14:22.644967-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: C++ \u09B8\u0982\u0996\u09CD\u09AF\
  \u09BE \u09AA\u09C2\u09B0\u09CD\u09A3\u09BE\u0999\u09CD\u0997 \u0995\u09B0\u09BE\
  \u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09C7\u09B6 \u0995\u09BF\u099B\u09C1 \u09AA\
  \u09A5 \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7, \u09AF\u09C7\u09AE\
  \u09A8 `floor()`, `ceil()`, \u098F\u09AC\u0982 `round()`."
lastmod: '2024-03-17T18:47:44.360129-06:00'
model: gpt-4-0125-preview
summary: "C++ \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09AA\u09C2\u09B0\u09CD\u09A3\u09BE\
  \u0999\u09CD\u0997 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09C7\
  \u09B6 \u0995\u09BF\u099B\u09C1 \u09AA\u09A5 \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8\
  \ \u0995\u09B0\u09C7, \u09AF\u09C7\u09AE\u09A8 `floor()`, `ceil()`, \u098F\u09AC\
  \u0982 `round()`."
title: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09A8\u09BF\u09B0\u09CD\u09A3\u09DF"
weight: 13
---

## কিভাবে:
C++ সংখ্যা পূর্ণাঙ্গ করার জন্য বেশ কিছু পথ প্রদান করে, যেমন `floor()`, `ceil()`, এবং `round()`:

```C++
#include <iostream>
#include <cmath> // পূর্ণাঙ্গ করার ফাংশনের জন্য

int main() {
    double num = 3.14;

    std::cout << "floor: " << std::floor(num) << "\n"; // আউটপুট: floor: 3
    std::cout << "ceil: " << std::ceil(num) << "\n";   // আউটপুট: ceil: 4
    std::cout << "round: " << std::round(num) << "\n"; // আউটপুট: round: 3

    // দুই দশমিকের জন্য নির্দিষ্ট নির্ভুলতা, যেমন:
    double precise_num = 3.146;
    double multiplier = 100.0;
    double rounded = std::round(precise_num * multiplier) / multiplier;

    std::cout << "দুই দশমিকে পূর্ণাঙ্গ: " << rounded << "\n"; // আউটপুট: দুই দশমিকে পূর্ণাঙ্গ: 3.15

    return 0;
}
```

## গভীর আলোচনা
C++11 আসার আগে, সংখ্যা পূর্ণাঙ্গ করা সাধারণত ম্যানুয়াল পদ্ধতি অথবা মানক বহির্ভূত লাইব্রেরির উপর নির্ভর করত। আজকে, `<cmath>` শক্তিশালী পদ্ধতি প্রদান করে। `floor()` নিচের দিকে পূর্ণাঙ্গ করে, `ceil()` উপরের দিকে পূর্ণাঙ্গ করে, অন্যদিকে `round()` নিকটতম পূর্ণসংখ্যায় যায়, 0.5 এর মতো পরিস্থিতি সামলানোর সময় সম সংখ্যায় পূর্ণাঙ্গ করে।

এই ফাংশনগুলির আচরণ বুঝতে পারা গুরুত্বপূর্ণ; উদাহরণ স্বরূপ, নেগেটিভ সংখ্যা (`std::round(-2.5)` yields `-2.0`) জটিলতা সৃষ্টি করতে পারে।

বিকল্প? ধনাত্মক সংখ্যার ক্ষেত্রে 0.5 যোগ করার পর একটি int এ রূপান্তর করা একটি প্রাচীন কৌশল ছিল কিন্তু নেগেটিভ সংখ্যার ক্ষেত্রে এবং টাইপ-অগ্নোস্টিক না হওয়ার দরুন ভুল হয়। বুস্ট এর মতো লাইব্রেরি আরও সূক্ষ্ম পদ্ধতি অফার করতে পারে, যেমন ভাষার এক্সটেনশন বা কম্পাইলার ইন্ট্রিনসিক নির্দিষ্ট হার্ডওয়্যারের জন্য অপ্টিমাইজ করতে পারে।

## আরো দেখুন
- `<cmath>` এর জন্য C++ রেফারেন্স: https://en.cppreference.com/w/cpp/header/cmath
- ফ্লোটিং-পয়েন্ট অঙ্কের জন্য IEEE মানদণ্ড (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- বুস্ট সংখ্যাতত্ত্ব রূপান্তর লাইব্রেরি: https://www.boost.org/doc/libs/release/libs/numeric/conversion/

---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:53.426753-06:00
description: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4\u09C7 \u0985\u09A5\u09AC\u09BE\
  \ \u0985\u09A4\u09C0\u09A4\u09C7\u09B0 \u098F\u0995\u099F\u09BF \u09A4\u09BE\u09B0\
  \u09BF\u0996 \u0997\u09A3\u09A8\u09BE \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7\
  \ \u0985\u09A6\u09C2\u09B0 \u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4\u09C7 \u09AC\
  \u09BE \u0997\u09A4 \u09B8\u09AE\u09AF\u09BC\u09C7\u09B0 \u098F\u0995\u099F\u09BF\
  \ \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09B8\u09AE\u09DF\u09B8\
  \u09C0\u09AE\u09BE \u09AA\u09B0 \u09AC\u09BE \u0986\u0997\u09C7 \u0995\u09C0 \u09A4\
  \u09BE\u09B0\u09BF\u0996 \u09B9\u09AC\u09C7 \u09A4\u09BE \u09A8\u09BF\u09B0\u09CD\
  \u09A3\u09AF\u09BC \u0995\u09B0\u09BE\u0964 \u098F\u099F\u09BF \u09B0\u09BF\u09AE\
  \u09BE\u0987\u09A8\u09CD\u09A1\u09BE\u09B0\u2026"
lastmod: '2024-03-17T18:47:44.378720-06:00'
model: gpt-4-0125-preview
summary: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4\u09C7 \u0985\u09A5\u09AC\u09BE\
  \ \u0985\u09A4\u09C0\u09A4\u09C7\u09B0 \u098F\u0995\u099F\u09BF \u09A4\u09BE\u09B0\
  \u09BF\u0996 \u0997\u09A3\u09A8\u09BE \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7\
  \ \u0985\u09A6\u09C2\u09B0 \u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4\u09C7 \u09AC\
  \u09BE \u0997\u09A4 \u09B8\u09AE\u09AF\u09BC\u09C7\u09B0 \u098F\u0995\u099F\u09BF\
  \ \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09B8\u09AE\u09DF\u09B8\
  \u09C0\u09AE\u09BE \u09AA\u09B0 \u09AC\u09BE \u0986\u0997\u09C7 \u0995\u09C0 \u09A4\
  \u09BE\u09B0\u09BF\u0996 \u09B9\u09AC\u09C7 \u09A4\u09BE \u09A8\u09BF\u09B0\u09CD\
  \u09A3\u09AF\u09BC \u0995\u09B0\u09BE\u0964 \u098F\u099F\u09BF \u09B0\u09BF\u09AE\
  \u09BE\u0987\u09A8\u09CD\u09A1\u09BE\u09B0 \u09A4\u09C8\u09B0\u09BF, \u09AE\u09C7\
  \u09DF\u09BE\u09A6 \u09A4\u09BE\u09B0\u09BF\u0996 \u09B8\u09C7\u099F \u0995\u09B0\
  \u09BE, \u0987\u09AD\u09C7\u09A8\u09CD\u099F \u09B8\u09BF\u09A1\u09BF\u0989\u09B2\
  \u09BF\u0982 \u0985\u09A5\u09AC\u09BE \u0995\u09A4\u099F\u09BE \u09B8\u09AE\u09DF\
  \ \u0985\u09A4\u09BF\u09AC\u09BE\u09B9\u09BF\u09A4 \u09B9\u09DF\u09C7\u099B\u09C7\
  \ \u09A4\u09BE \u09B2\u0997 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u0989\
  \u09AA\u0995\u09BE\u09B0\u09C0\u0964."
title: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u09AC\u09BE \u0985\u09A4\u09C0\
  \u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u0997\u09A3\u09A8\u09BE \u0995\
  \u09B0\u09BE"
weight: 26
---

## কিভাবে:
C++20 আমাদের কাছে `<chrono>` লাইব্রেরির উন্নতি নিয়ে আসে, ফলে সময় নিয়ে কাজ করা এখন আর ঝামেলার নয়। এখানে একটি দ্রুত উদাহরণ দেখানো হল বর্তমান তারিখে কয়েক দিন যোগ করা হচ্ছে:

```C++
#include <iostream>
#include <chrono>
#include <format>

using namespace std::chrono;

int main() {
    // আজকের তারিখ নেওয়া
    auto today = floor<days>(system_clock::now());
    
    // আজকের তারিখে 30 দিন যোগ করা
    auto future_date = today + days(30);
    
    // system_clock ব্যবহার করে আউটপুট দেওয়ার জন্য time_point এ পরিণত করা
    auto tp = system_clock::time_point(future_date);
    
    // আউটপুট
    std::cout << "আজকের তারিখ: "
              << std::format("{:%F}\n", today);
    std::cout << "ভবিষ্যতে তারিখ (30 দিন পরে): "
              << std::format("{:%F}\n", tp);
    return 0;
}
```

নমুনা আউটপুট:
```
আজকের তারিখ: 2023-03-15
ভবিষ্যতে তারিখ (30 দিন পরে): 2023-04-14
```

দিনগুলি বিয়োগ করা একইরকমভাবে কাজ করে—আপনি শুধু `+` এর পরিবর্তে `-` ব্যবহার করবেন।

## গভীর ডুব
C++20 আসার আগে, আপনি হয়তো বুস্টের মত লাইব্রেরি ব্যবহার করতেন তারিখগুলি ম্যানিপুলেট করার জন্য। কিন্তু আপডেট হওয়া `<chrono>` `system_clock`, `year_month_day`, এবং `duration` প্রকারের সাথে এটি সরল করে তোলে।

ঐতিহাসিকভাবে, তারিখ গণনা জটিল ছিল বিভিন্ন মাসের দৈর্ঘ্য, অধিবর্ষ এবং সময় অঞ্চলগুলির ম্যানুয়াল হ্যান্ডলিং এর কারণে। C++20 এর `<chrono>` এই সমস্যা সমাধান করে ক্যালেন্ডার ও সময় অঞ্চল সমর্থন প্রদান করে।

বিকল্প? আপনি এখনও বুস্ট ব্যবহার করতে পারেন বা নিজের তারিখ যুক্তি হাতে তৈরি করতে পারেন (অভিযানপূর্ণ, কিন্তু কেন?)। এছাড়া Howard Hinnant's "date" লাইব্রেরি মত তৃতীয়-পক্ষের লাইব্রেরী আছে, যা C++20 চর্ণো আপডেটে প্রভাবশালী ছিল।

বাস্তবায়নের দিক থেকে, `<chrono>` মেয়াদকে কম্পাইল-সময়ের যুক্তিসংগত স্থিরাংক হিসেবে সংজ্ঞায়িত করে, ফ্লোটিং-পয়েন্টের সমস্যা এড়িয়ে চলে। `year_month_day` প্রকারটি `sys_days` এ নির্ভর করে, যা একটি সাধারণ যুগ (1970-01-01) থেকে দিনগুলি হিসেবে একটি time_point প্রতিনিধিত্ব করে।

## আরও দেখুন
- `chrono` এর জন্য C++ রেফারেন্স: https://en.cppreference.com/w/cpp/header/chrono
- Howard Hinnant's Date লাইব্রেরি (C++20 এর chrono আপডেটের পূর্বযুগ): https://github.com/HowardHinnant/date
- Boost ডেট/টাইমের ডকুমেন্টেশন: https://www.boost.org/doc/libs/release/libs/date_time/

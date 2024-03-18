---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:40.963066-06:00
description: "C++-\u098F \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\
  \u09BF\u0996 \u09AA\u09C7\u09A4\u09C7 \u09AF\u09BE\u09AC\u09BE\u09B0 \u09AA\u09CD\
  \u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE \u098F\u09AE\u09A8 \u09B8\u09AC\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09C7\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u09AE\u09CC\u09B2\u09BF\u0995 \u0995\u09BE\u099C \u09AF\u09C7\u0997\
  \u09C1\u09B2\u09CB\u09B0 \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u09B9\
  \u09DF \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7\u09B0 \u0998\u09A1\u09BC\
  \u09BF\u09B0 \u09AD\u09BF\u09A4\u09CD\u09A4\u09BF\u09A4\u09C7 \u09A4\u09BE\u09B0\
  \u09BF\u0996 \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE \u0995\
  \u09B0\u09BE\u09B0 \u0985\u09A5\u09AC\u09BE\u2026"
lastmod: '2024-03-17T18:47:44.375813-06:00'
model: gpt-4-0125-preview
summary: "C++-\u098F \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\
  \u09BF\u0996 \u09AA\u09C7\u09A4\u09C7 \u09AF\u09BE\u09AC\u09BE\u09B0 \u09AA\u09CD\
  \u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE \u098F\u09AE\u09A8 \u09B8\u09AC\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09C7\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u09AE\u09CC\u09B2\u09BF\u0995 \u0995\u09BE\u099C \u09AF\u09C7\u0997\
  \u09C1\u09B2\u09CB\u09B0 \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u09B9\
  \u09DF \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7\u09B0 \u0998\u09A1\u09BC\
  \u09BF\u09B0 \u09AD\u09BF\u09A4\u09CD\u09A4\u09BF\u09A4\u09C7 \u09A4\u09BE\u09B0\
  \u09BF\u0996 \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE \u0995\
  \u09B0\u09BE\u09B0 \u0985\u09A5\u09AC\u09BE\u2026"
title: "\u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u09AA\u09C7\u09A4\u09C7"
---

{{< edit_this_page >}}

## কি এবং কেন?
C++-এ বর্তমান তারিখ পেতে যাবার প্রক্রিয়া এমন সব প্রোগ্রামের জন্য মৌলিক কাজ যেগুলোর প্রয়োজন হয় সিস্টেমের ঘড়ির ভিত্তিতে তারিখ প্রক্রিয়া করার অথবা দেখানোর। লগ করা, সময় স্ট্যাম্পিং, শিডিউল করা কাজ, এবং যে কোনো কাজ যা তারিখ ও সময়ের উপর নির্ভর করে, এর জন্য এটা প্রয়োজনীয়।

## কিভাবে:
C++ বর্তমান তারিখ পেতে বিভিন্ন উপায় প্রদান করে, যার মধ্যে রয়েছে C++ স্ট্যান্ডার্ড লাইব্রেরি এবং তৃতীয় পক্ষের লাইব্রেরির মতো বুস্ট। নিম্নের উদাহরণগুলি এই কাজটি কীভাবে সম্পাদন করা যায় তা দেখায়।

### `<chrono>` ব্যবহার করে (C++20 এবং পরবর্তী)
C++20 `<chrono>` লাইব্রেরিতে আরও বেশি কার্যকারিতা প্রদান করেছে, যা বর্তমান তারিখ পেতে সহজ করে তোলে:
```cpp
#include <iostream>
#include <chrono>
#include <format> // std::format এর জন্য (C++20)

int main() {
    auto current_time_point = std::chrono::system_clock::now(); // বর্তমান সময় ক্যাপচার করুন
    auto current_time_t = std::chrono::system_clock::to_time_t(current_time_point); // time_t তে রূপান্তর করুন

    // সময়কে একটি পঠনযোগ্য বিন্যাসে বিন্যাস করুন
    std::cout << "বর্তমান তারিখ: " << std::format("{:%Y-%m-%d}", std::chrono::system_clock::to_time_t(current_time_point)) << std::endl;

    return 0;
}
```
**নমুনা আউটপুট:**
```plaintext
বর্তমান তারিখ: 2023-03-15
```

### `<ctime>` ব্যবহার করে
C++-এর পুরনো সংস্করণের সাথে কাজ করা প্রোগ্রামারদের জন্য বা যারা প্রথাগত C লাইব্রেরি পছন্দ করেন:
```cpp
#include <iostream>
#include <ctime>

int main() {
    std::time_t t = std::time(0); // বর্তমান সময় পান
    std::tm* now = std::localtime(&t);
    std::cout << "বর্তমান তারিখ: " 
              << (now->tm_year + 1900) << '-' 
              << (now->tm_mon + 1) << '-'
              <<  now->tm_mday
              << std::endl;

    return 0;
}
```
**নমুনা আউটপুট:**
```plaintext
বর্তমান তারিখ: 2023-03-15
```

### Boost Date_Time ব্যবহার করে
যে সকল প্রজেক্ট বুস্ট লাইব্রেরি ব্যবহার করে, বুস্ট Date_Time লাইব্রেরি বর্তমান তারিখ পেতে একটি বিকল্প পদ্ধতি প্রদান করে:
```cpp
#include <iostream>
#include <boost/date_time.hpp>

int main() {
    // বুস্টের গ্রেগোরিয়ান ক্যালেন্ডার ব্যবহার করে বর্তমান দিন পাওয়া
    boost::gregorian::date today = boost::gregorian::day_clock::local_day();
    std::cout << "বর্তমান তারিখ: " << today << std::endl;

    return 0;
}
```
**নমুনা আউটপুট:**
```plaintext
বর্তমান তারিখ: 2023-Mar-15
```
এই উদাহরণগুলি C++-এ তারিখের সাথে কাজ করার জন্য মৌলিক ভিত্তি প্রদান করে, যা বিভিন্ন ধরনের অ্যাপ্লিকেশনের জন্য প্রয়োজনীয়।

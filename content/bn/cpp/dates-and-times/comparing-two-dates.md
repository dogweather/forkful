---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:27.431479-06:00
description: "\u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\u09B0\
  \ \u09A4\u09C1\u09B2\u09A8\u09BE \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09AC\
  \u09CB\u099D\u09BE\u09B0 \u099A\u09C7\u09B7\u09CD\u099F\u09BE \u0995\u09B0\u09BE\
  \ \u0995\u09CB\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996\u099F\u09BF \u0986\u0997\u09C7\
  \u09B0, \u0995\u09CB\u09A8\u099F\u09BF \u09AA\u09B0\u09C7\u09B0, \u0985\u09A5\u09AC\
  \u09BE \u09A4\u09BE\u09B0\u09BE \u09AF\u09A6\u09BF \u098F\u0995\u0987 \u09B9\u09AF\
  \u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\
  \u09BE \u098F\u099F\u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u09A8 \u0987\
  \u09AD\u09C7\u09A8\u09CD\u099F\u0997\u09C1\u09B2\u09CB \u0986\u09AF\u09BC\u09CB\u099C\
  \u09A8,\u2026"
lastmod: '2024-03-17T18:47:44.377764-06:00'
model: gpt-4-0125-preview
summary: "\u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\u09B0 \u09A4\
  \u09C1\u09B2\u09A8\u09BE \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09AC\u09CB\
  \u099D\u09BE\u09B0 \u099A\u09C7\u09B7\u09CD\u099F\u09BE \u0995\u09B0\u09BE \u0995\
  \u09CB\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996\u099F\u09BF \u0986\u0997\u09C7\u09B0\
  , \u0995\u09CB\u09A8\u099F\u09BF \u09AA\u09B0\u09C7\u09B0, \u0985\u09A5\u09AC\u09BE\
  \ \u09A4\u09BE\u09B0\u09BE \u09AF\u09A6\u09BF \u098F\u0995\u0987 \u09B9\u09AF\u09BC\
  \u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u098F\u099F\u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u09A8 \u0987\u09AD\
  \u09C7\u09A8\u09CD\u099F\u0997\u09C1\u09B2\u09CB \u0986\u09AF\u09BC\u09CB\u099C\u09A8\
  ,\u2026"
title: "\u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u09A4\u09C1\u09B2\
  \u09A8\u09BE \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
দুটি তারিখের তুলনা করা মানে বোঝার চেষ্টা করা কোন তারিখটি আগের, কোনটি পরের, অথবা তারা যদি একই হয়। প্রোগ্রামাররা এটি করে থাকেন ইভেন্টগুলো আয়োজন, প্রোমোশনের মেয়াদ উত্তীর্ণ হওয়া, সূচিবদ্ধ করা, রিমাইন্ডার—মূলত, কোনও সময়ের উপাদান সম্বলিত যেকোনো কিছু।

## কিভাবে:
C++ `<chrono>` হেডারের সাহায্যে জীবনকে সহজ করে তোলে।

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    using namespace std::chrono;

    // system_clock সময় পয়েন্টস তৈরি করা
    system_clock::time_point today = system_clock::now();
    system_clock::time_point someDay = system_clock::now() - hours(24); // গতকাল

    // তুলনার জন্য time_t এ রূপান্তর
    time_t today_time_t = system_clock::to_time_t(today);
    time_t someDay_time_t = system_clock::to_time_t(someDay);

    if (today_time_t > someDay_time_t) {
        std::cout << "Today is after someDay.\n";
    } else if (today_time_t < someDay_time_t) {
        std::cout << "Today is before someDay.\n";
    } else {
        std::cout << "Dates are the same.\n";
    }

    return 0;
}
```

উদাহরণ আউটপুট:

```
Today is after someDay.
```

## গভীর ডুব:
C++11 থেকে, `<chrono>` এটা সময় এবং তারিখের জন্য জায়গা। তার পূর্বে, আপনি সম্ভবত `<ctime>` এবং `tm` এর মতো structs এর সাথে জ্বালাময়ী অবস্থায় ছিলেন। এটা সুন্দর ছিল না।

বিকল্প? অবশ্যই, তৃতীয়-পক্ষের লাইব্রেরিগুলো রয়েছে যেমন Boost.DateTime। কিন্তু `<chrono>` যখন ঠিক সেখানে রয়েছে এবং এটি বিকশিত হচ্ছে তখন কেন জটিল করবেন?

মনে রাখার মতো বাস্তবায়নের বিস্তারিতগুলো:
- `std::chrono` সময় পয়েন্টস এবং সময়কালের সাথে নিয়োজিত।
- `system_clock` বাস্তব বিশ্বের সময় মাপে।
- `time_point` একটি নির্দিষ্ট সময়ের বিন্দু (যেমন, একটি তারিখ)।
- `time_t` হল একটি অঙ্কনীয় প্রকার, তুলনার জন্য সুবিধাজনক।

## আরও দেখুন:
- `<chrono>` এর জন্য C++ রেফারেন্স: https://en.cppreference.com/w/cpp/header/chrono
- তারিখ এবং সময়ের লাইব্রেরিগুলির তুলনা: http://www.boost.org/doc/libs/1_64_0/doc/html/date_time.html
- যদি আপনি নস্টালজিক অথবা ম্যাসোকিস্টিক অনুভব করেন তবে ভালো পুরোনো `<ctime>`: https://en.cppreference.com/w/cpp/header/ctime

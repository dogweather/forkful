---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:21.944536-06:00
description: "\u09B8\u09BF++ \u098F \u098F\u0995\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\
  \u0996\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0986\u0995\u09BE\
  \u09B0\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE\
  \ \u09AE\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u0985\u09AC\u099C\u09C7\u0995\u09CD\u099F\u0995\u09C7 \u09AA\u09A0\u09A8\u09AF\
  \u09CB\u0997\u09CD\u09AF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09B0\u09AE\
  \u09CD\u09AF\u09BE\u099F\u09C7 \u09AA\u09B0\u09BF\u09A3\u09A4 \u0995\u09B0\u09BE\
  \u0964 \u098F\u099F\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\
  \u09B0\u09C0\u09A6\u09C7\u09B0 \u0995\u09BE\u099B\u09C7 \u09A4\u09BE\u09B0\u09BF\
  \u0996 \u09AA\u09CD\u09B0\u09A6\u09B0\u09CD\u09B6\u09A8 \u098F\u09AC\u0982\u2026"
lastmod: '2024-03-17T18:47:44.376799-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09BF++ \u098F \u098F\u0995\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996\
  \u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0986\u0995\u09BE\u09B0\
  \u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE \u09AE\
  \u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u0985\
  \u09AC\u099C\u09C7\u0995\u09CD\u099F\u0995\u09C7 \u09AA\u09A0\u09A8\u09AF\u09CB\u0997\
  \u09CD\u09AF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09B0\u09AE\u09CD\u09AF\
  \u09BE\u099F\u09C7 \u09AA\u09B0\u09BF\u09A3\u09A4 \u0995\u09B0\u09BE\u0964 \u098F\
  \u099F\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\u09C0\u09A6\
  \u09C7\u09B0 \u0995\u09BE\u099B\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996 \u09AA\u09CD\
  \u09B0\u09A6\u09B0\u09CD\u09B6\u09A8 \u098F\u09AC\u0982\u2026"
title: "\u09A4\u09BE\u09B0\u09BF\u0996\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u098F \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কী এবং কেন?
সি++ এ একটি তারিখকে স্ট্রিং আকারে রূপান্তর করা মানে একটি তারিখ অবজেক্টকে পঠনযোগ্য টেক্সট ফরম্যাটে পরিণত করা। এটি ব্যবহারকারীদের কাছে তারিখ প্রদর্শন এবং মানব-বান্ধব উপায়ে ইভেন্টগুলি লগ করার জন্য মুখ্য।

## কীভাবে:
আধুনিক সি++ এ, `<chrono>` এবং `<iomanip>` লাইব্রেরিগুলি আপনার জন্য ডেট-টাইম অপারেশনের জন্য বন্ধু। এখানে `std::put_time` ব্যবহার করে একটি দ্রুত পদ্ধতি দেওয়া হল:

```cpp
#include <iostream>
#include <iomanip>
#include <chrono>
#include <sstream>

int main() {
    auto now = std::chrono::system_clock::now(); // বর্তমান সময় পেতে
    auto time = std::chrono::system_clock::to_time_t(now); // time_t এ রূপান্তর
    
    // ফরম্যাটিং এর জন্য tm স্ট্রাক্চারে রূপান্তর
    std::tm tm = *std::localtime(&time);

    // আউটপুটের জন্য স্ট্রিং স্ট্রীম
    std::stringstream ss;

    ss << std::put_time(&tm, "%Y-%m-%d %H:%M:%S"); // ফর্ম্যাট: YYYY-MM-DD HH:MM:SS

    std::string date_str = ss.str(); // স্ট্রিং এ রূপান্তর

    std::cout << date_str << std::endl; // তারিখের স্ট্রিং আউটপুট করুন
    return 0;
}
```

নমুনা আউটপুট (বর্তমান তারিখ এবং সময়ের উপর নির্ভর করে):
```
2023-03-15 14:25:30
```

## বিস্তারিত আলোচনা
`<chrono>` এর মধ্যে এসেছে আগে, সি++ প্রোগ্রামারদের প্রায়ই `<ctime>` এর মাধ্যমে সি-স্টাইল সময় হ্যান্ডলিং নিয়ে লড়াই করতে হতো। এটি কম সহজবোধ্য এবং ম্যানুয়াল মেমরি ম্যানেজমেন্ট এবং প্ল্যাটফর্ম-নির্ভর সমস্যাগুলির কারণে আরও ত্রুটিপূর্ণ ছিল।

`std::put_time` এর বিকল্পগুলির মধ্যে `strftime` ব্যবহার করা অন্তর্ভুক্ত, তবে এটি আরও সি-স্টাইল। তৃতীয়-পক্ষের লাইব্রেরিগুলি যেমন Boost.Date_Time নির্ভরতা যোগ করার খরচে আরও ফাংশনালিটি অফার করতে পারে।

`std::put_time` এর ফরম্যাট স্পেসিফায়ারগুলি বুঝতে একটি মুখ্য বাস্তব বিস্তারিত হল, যেগুলি `strftime` এ ব্যবহৃতদের অনুরূপ। আপনি প্লেসহোল্ডারগুলিকে তারিখ বা সময়ের উপাদানগুলি — `%Y` পূর্ণ বছরের জন্য, `%m` মাসের জন্য, এবং তাই ম্যাপিং করছেন।

## দেখুন আরও
- [`<chrono>` ডকুমেন্টেশন](https://en.cppreference.com/w/cpp/header/chrono)
- [`<iomanip>` ডকুমেন্টেশন](https://en.cppreference.com/w/cpp/header/iomanip)
- [Boost.Date_Time](https://www.boost.org/doc/libs/release/libs/date_time/)

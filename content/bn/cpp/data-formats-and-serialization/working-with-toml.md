---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:30:24.702890-06:00
description: "TOML (\u099F\u09AE\u09C7\u09B0 \u09B8\u09CD\u09AA\u09B7\u09CD\u099F\
  , \u09A8\u09CD\u09AF\u09C2\u09A8\u09A4\u09AE \u09AD\u09BE\u09B7\u09BE) \u098F\u0995\
  \u099F\u09BF \u09A1\u09C7\u099F\u09BE \u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\
  \u09B2\u09BE\u0987\u099C\u09C7\u09B6\u09A8 \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F\
  \ \u09AF\u09BE \u098F\u09B0 \u09B8\u09CD\u09AA\u09B7\u09CD\u099F \u09B8\u09BF\u09AE\
  \u09BE\u09A8\u09CD\u099F\u09BF\u0995\u09CD\u09B8\u09C7\u09B0 \u0995\u09BE\u09B0\u09A3\
  \u09C7 \u09AA\u09A1\u09BC\u09BE \u09B8\u09B9\u099C\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09AE\u09BE\u09A8\u09AC\
  \ \u09AA\u09BE\u09A0\u09AF\u09CB\u0997\u09CD\u09AF\u09A4\u09BE \u098F\u09AC\u0982\
  \ \u09AE\u09C7\u09B6\u09BF\u09A8\u2026"
lastmod: '2024-03-17T18:47:44.388711-06:00'
model: gpt-4-0125-preview
summary: "TOML (\u099F\u09AE\u09C7\u09B0 \u09B8\u09CD\u09AA\u09B7\u09CD\u099F, \u09A8\
  \u09CD\u09AF\u09C2\u09A8\u09A4\u09AE \u09AD\u09BE\u09B7\u09BE) \u098F\u0995\u099F\
  \u09BF \u09A1\u09C7\u099F\u09BE \u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\u09B2\
  \u09BE\u0987\u099C\u09C7\u09B6\u09A8 \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F\
  \ \u09AF\u09BE \u098F\u09B0 \u09B8\u09CD\u09AA\u09B7\u09CD\u099F \u09B8\u09BF\u09AE\
  \u09BE\u09A8\u09CD\u099F\u09BF\u0995\u09CD\u09B8\u09C7\u09B0 \u0995\u09BE\u09B0\u09A3\
  \u09C7 \u09AA\u09A1\u09BC\u09BE \u09B8\u09B9\u099C\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09AE\u09BE\u09A8\u09AC\
  \ \u09AA\u09BE\u09A0\u09AF\u09CB\u0997\u09CD\u09AF\u09A4\u09BE \u098F\u09AC\u0982\
  \ \u09AE\u09C7\u09B6\u09BF\u09A8 \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u09AC\u09BF\
  \u09B2\u09BF\u099F\u09BF\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u098F\u0995\u099F\
  \u09BF \u09AD\u09BE\u09B0\u09B8\u09BE\u09AE\u09CD\u09AF \u09B0\u09BE\u0996\u09BE\
  \u09B0 \u099C\u09A8\u09CD\u09AF \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\
  \u09B6\u09A8 \u09AB\u09BE\u0987\u09B2\u0997\u09C1\u09B2\u09BF\u09B0 \u099C\u09A8\
  \u09CD\u09AF TOML \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\u0964\
  ."
title: "\u099F\u09AE\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\
  \u09B0\u09BE"
weight: 39
---

## কিভাবে:
C++ এ TOML নিয়ে কাজ করতে, আপনার `toml++` এর মত একটি লাইব্রেরি প্রয়োজন হবে। এখানে একটি দ্রুত শুরু রয়েছে:

```C++
#include <toml++/toml.h>
#include <iostream>
#include <fstream>

int main() {
    // ফাইল থেকে TOML পার্স করা
    std::ifstream ifs("config.toml");
    auto config = toml::parse(ifs);

    // একটি মান অ্যাক্সেস করা
    std::string title = config["title"].value_or("Untitled");
    std::cout << "শিরোনাম: " << title << '\n';

    // পরিবর্তন এবং TOML সংরক্ষণ করা
    config["title"] = "নতুন শিরোনাম";
    std::ofstream ofs("config.toml");
    ofs << config;
}
```

`config.toml` নমুনা:
```toml
title = "উদাহরণ"
```

নমুনা আউটপুট:
```plaintext
শিরোনাম: উদাহরণ
```

## গভীর ডুব
TOML টম প্রেস্টন-ওয়ের্নার দ্বারা ২০১৩ সালে YAML এবং JSON এর বিকল্প হিসেবে তৈরি করা হয়েছিল। এটি মূলত কনফিগারেশন ফাইলগুলির জন্য সরল এবং স্বস্তিদায়ক হতে ডিজাইন করা হয়েছিল। JSON এর মতো নয়, TOML অস্পষ্টতা এড়ানোর উপর মনোনিবেশ করে, যা মানে এটি নথির পার্স করার পদ্ধতি স্থির।

TOML এর বিকল্পগুলির মধ্যে YAML রয়েছে, যা অনুমোদিত বিষয়গুলির বেলায় আরও উদার, যদিও কখনও কখনও প্রেডিক্টেবিলিটির খরচে। অন্য বিকল্প JSON, যা কাঠামোর দিক থেকে বেশ কঠোর তবে মন্তব্য ও এর বন্ধনী ভারী সিনট্যাক্সের অভাবের কারণে কনফিগারেশনের জন্য মানুষের জন্য সহজতর নয়।

বাস্তবায়নে, `toml++` একটি হেডার-অনলি C++17 লাইব্রেরি যা সর্বশেষ TOML স্পেসিফিকেশনের সাথে সামঞ্জস্যপূর্ণ। এটি প্রজেক্টে সহজেই একীভূত করতে TOML ডেটা নেভিগেট এবং ম্যানিপুলেট করার জন্য একটি DOM-এর মতো ইন্টারফেস প্রদান করে। লাইব্রেরিটি C++ টাইপ ব্যবহার করে TOML ডেটা পাওয়া ও সেট করার জন্য পার্সিং, যাচাই এবং আউটপুট জেনারেশনের দেখভাল করে।

## আরও দেখুন
- TOML GitHub রিপোজিটরি: https://github.com/toml-lang/toml
- `toml++`, C++ এর জন্য একটি TOML লাইব্রেরি: https://github.com/marzer/tomlplusplus
- ফরম্যাটের বিস্তারিত ব্যাখ্যা সহ অফিশিয়াল TOML ডকুমেন্টেশন: https://toml.io/en/

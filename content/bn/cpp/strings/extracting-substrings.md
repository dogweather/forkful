---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:04.387741-06:00
description: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u0995\
  \u09CD\u09B8\u099F\u09CD\u09B0\u09BE\u0995\u09CD\u099F \u0995\u09B0\u09BE \u09AE\
  \u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09AC\u09A1\u09BC \u09B8\u09CD\u099F\
  \u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u099B\u09CB\u099F \u099B\u09CB\
  \u099F \u099F\u09C1\u0995\u09B0\u09CB \u09AC\u09C7\u09B0 \u0995\u09B0\u09BE\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u098F\u099F\u09BF \u0995\u09B0\u09C7\u09A8 \u099F\u09C7\u0995\u09CD\u09B8\u099F\
  \u09C7\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\
  \u09B7\u09CD\u099F \u09A1\u09BE\u099F\u09BE, \u09AF\u09C7\u09AE\u09A8 \u0987\u09AE\
  \u09C7\u0987\u09B2 \u09A0\u09BF\u0995\u09BE\u09A8\u09BE \u09A5\u09C7\u0995\u09C7\
  \u2026"
lastmod: '2024-03-17T18:47:44.353519-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u0995\
  \u09CD\u09B8\u099F\u09CD\u09B0\u09BE\u0995\u09CD\u099F \u0995\u09B0\u09BE \u09AE\
  \u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09AC\u09A1\u09BC \u09B8\u09CD\u099F\
  \u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u099B\u09CB\u099F \u099B\u09CB\
  \u099F \u099F\u09C1\u0995\u09B0\u09CB \u09AC\u09C7\u09B0 \u0995\u09B0\u09BE\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u098F\u099F\u09BF \u0995\u09B0\u09C7\u09A8 \u099F\u09C7\u0995\u09CD\u09B8\u099F\
  \u09C7\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\
  \u09B7\u09CD\u099F \u09A1\u09BE\u099F\u09BE, \u09AF\u09C7\u09AE\u09A8 \u0987\u09AE\
  \u09C7\u0987\u09B2 \u09A0\u09BF\u0995\u09BE\u09A8\u09BE \u09A5\u09C7\u0995\u09C7\
  \ \u0987\u0989\u099C\u09BE\u09B0\u09A8\u09C7\u09AE \u09AC\u09BE \u09B2\u0997 \u09A5\
  \u09C7\u0995\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996 \u09AC\u09C7\u09B0 \u0995\u09B0\
  \u09C7 \u0986\u09B2\u09BE\u09A6\u09BE, \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\
  \u09AF\u09BC\u09BE\u099C\u09BE\u09A4 \u0995\u09B0\u09BE \u0985\u09A5\u09AC\u09BE\
  \ \u09AC\u09BF\u09B6\u09CD\u09B2\u09C7\u09B7\u09A3 \u0995\u09B0\u09BE\u09B0 \u099C\
  \u09A8\u09CD\u09AF\u0964."
title: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AC\u09C7\u09B0\
  \ \u0995\u09B0\u09BE"
weight: 6
---

## কিভাবে:
C++ এ একটি সাবস্ট্রিং ধরা খুবই সহজ। `std::string` এখানে আমাদের বিশ্বস্ত সহায়ক, `substr()` ফাংশন এখানে অধিকাংশ ভারী কাজ সম্পাদন করে। আসুন, কিছু কোডের মাধ্যমে এটি দেখে নিই:

```C++
#include <iostream>
#include <string>

int main() {
    std::string fullString = "Hello, World! Programming in C++ is fun.";
    std::string snippet;

    // ইনডেক্স 7 থেকে শুরু করে "World" এক্সট্রাক্ট করুন, দৈর্ঘ্য 5
    snippet = fullString.substr(7, 5);
    std::cout << snippet << std::endl; // আউটপুট: World

    // ইনডেক্স 14 থেকে শুরু করে "Programming" এক্সট্রাক্ট করুন
    snippet = fullString.substr(14);
    std::cout << snippet << std::endl; // আউটপুট: Programming in C++ is fun.

    return 0;
}
```

## গভীর ডাইভ
সাবস্ট্রিং নতুন কিছু নয়। পুরোনো স্কুলের C প্রোগ্রামাররা `strncpy` এবং ম্যানুয়াল বইপত্র ব্যবহার করতেন। স্ট্রিং হ্যান্ডলিং বাগের একটি সাধারণ ব্রিড, তাই C++ এটিকে সহজ করতে চেয়েছিল। `std::string` এবং এর `substr` মেথড C++98 এর সময় থেকে এসেছে এবং স্ট্রেস কমাচ্ছে।

বিকল্প? অবশ্যই। আপনি ম্যানুয়াল `std::string::iterator` ব্যবহার করতে পারেন অথবা C ফাংশন ধুলো ঝাড়া যেতে পারে—যদি আপনি বিপদজনকভাবে বাঁচতে পছন্দ করেন। আরো আধুনিক পদ্ধতি হিসেবে string_views না-পরিবর্তনকারী উদ্দীপনা জন্য থাকতে পারে।

বাস্তবায়ন? অন্তরাত্মায়, `substr` প্রায়শই নতুন স্টোরেজ বরাদ্দ এবং ডেটা কপি করে, যা বিনামূল্যে নয়। পুরানো সময়ের কাঁচা পয়েন্টারগুলি এবং char arrays এর সাথে কুস্তি তুলনায় এটি হালকা, কিন্তু এটি মুহূর্তের নয়।

## আরো দেখুন
`std::string` এবং এর বন্ধুদের সম্পর্কে আরো জানতে:
- cppreference.com এ `std::string` নিয়ে: https://en.cppreference.com/w/cpp/string/basic_string
- `std::string_view` নিয়ে আরো জানুন: https://en.cppreference.com/w/cpp/string/basic_string_view
- C-স্টাইল স্ট্রিং হ্যান্ডলিং (ঐতিহাসিক কৌতূহলের জন্য): http://www.cplusplus.com/reference/cstring/

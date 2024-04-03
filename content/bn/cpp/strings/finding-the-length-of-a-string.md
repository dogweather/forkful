---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:03.972578-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u0995\u09B0\u09AC\u09C7\u09A8\
  : C++ \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09DF\u09C7\
  \u09B0 \u09A6\u09C8\u09B0\u09CD\u0998\u09CD\u09AF \u09AC\u09C7\u09B0 \u0995\u09B0\
  \u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF \u09B8\u09B0\u09B2\
  \ \u09AA\u09A5 \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7 `std::string`\
  \ \u0995\u09CD\u09B2\u09BE\u09B8\u09C7\u09B0 `length()` \u09AE\u09C7\u09A5\u09A1\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\u0964 \u0995\u09BF\
  \u09A8\u09CD\u09A4\u09C1 \u09AF\u09A6\u09BF \u0986\u09AA\u09A8\u09BF \u09AA\u09C1\
  \u09B0\u09BE\u09A8\u09CB\u2026"
lastmod: '2024-03-17T18:47:44.355681-06:00'
model: gpt-4-0125-preview
summary: "C++ \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09DF\
  \u09C7\u09B0 \u09A6\u09C8\u09B0\u09CD\u0998\u09CD\u09AF \u09AC\u09C7\u09B0 \u0995\
  \u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF \u09B8\u09B0\
  \u09B2 \u09AA\u09A5 \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7 `std::string`\
  \ \u0995\u09CD\u09B2\u09BE\u09B8\u09C7\u09B0 `length()` \u09AE\u09C7\u09A5\u09A1\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\u0964 \u0995\u09BF\
  \u09A8\u09CD\u09A4\u09C1 \u09AF\u09A6\u09BF \u0986\u09AA\u09A8\u09BF \u09AA\u09C1\
  \u09B0\u09BE\u09A8\u09CB \u09A7\u09BE\u09B0\u09BE \u0985\u09A8\u09C1\u09B8\u09B0\
  \u09A3 \u0995\u09B0\u09C7\u09A8, \u09A4\u09BE\u09B9\u09B2\u09C7 C-\u09B8\u09CD\u099F\
  \u09BE\u0987\u09B2 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09AC\u0982\
  \ `strlen()` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\u09C7\
  \ \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u09A6\u09C1\
  \u099F\u09BF \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09DF\u09BE\
  \ \u09B9\u09DF\u09C7\u099B\u09C7."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09A6\u09C8\u09B0\
  \u09CD\u0998\u09CD\u09AF \u099A\u09BF\u09B9\u09CD\u09A8\u09BF\u09A4 \u0995\u09B0\
  \u09BE"
weight: 7
---

## কিভাবে করবেন:
C++ একটি স্ট্রিংয়ের দৈর্ঘ্য বের করার জন্য একটি সরল পথ প্রদান করে `std::string` ক্লাসের `length()` মেথড ব্যবহার করে। কিন্তু যদি আপনি পুরানো ধারা অনুসরণ করেন, তাহলে C-স্টাইল স্ট্রিং এবং `strlen()` ব্যবহার করতে পারেন। এখানে দুটি উদাহরণ দেওয়া হয়েছে:

```C++
#include <iostream>
#include <string>
#include <cstring>

int main() {
    // std::string ব্যবহার করে
    std::string greeting = "Hello, World!";
    std::cout << "স্ট্রিংয়ের দৈর্ঘ্য (std::string): " << greeting.length() << std::endl;

    // C-স্টাইল স্ট্রিং ব্যবহার করে
    const char *c_greeting = "Hello, World!";
    std::cout << "স্ট্রিংয়ের দৈর্ঘ্য (C-স্টাইল): " << strlen(c_greeting) << std::endl;

    return 0;
}
```

নমুনা আউটপুট:
```
স্ট্রিংয়ের দৈর্ঘ্য (std::string): 13
স্ট্রিংয়ের দৈর্ঘ্য (C-স্টাইল): 13
```

## গভীর ডুব:
মূলত, C++ C-স্টাইল ক্যারেক্টার অ্যারে এবং তার সাথে যুক্ত `strlen()` ফাংশনকে C থেকে উত্তরসূরি করে নিয়েছে। `strlen()` ফাংশনটি নাল ক্যারেক্টার `'\0'` এ পৌঁছানোর জন্য অ্যারের মধ্যে দিয়ে এগিয়ে যায়, এবং তার মাধ্যমে দৈর্ঘ্য নির্ণয় করে। এটি একটি সাধারণ কিন্তু কার্যকরী কৌশল, তবে `std::string.length()` এর দক্ষতা ছাড়িয়ে যাওয়া সম্ভব নয়, যা সাধারণত দ্রুত পুনরুদ্ধারের জন্য দৈর্ঘ্য ট্র্যাক করে রাখে।

বিকল্প পথ? অবশ্যই আছে:
- আপনি আরও `size()` মেথড ব্যবহার করতে পারেন, যা `std::string` এর জন্য `length()` এর সমান।
- ওয়াইড ক্যারেক্টার স্ট্রিংয়ের জন্য, `std::wstring` এবং এর `length()` মেথড আপনার বন্ধু।
- মশলাদার বিকল্প হিসেবে কাস্টম ফাংশন অথবা ইটারেটরস সাথে `std::distance` মত এলগরিদম ব্যবহার করা যায়।

তবে সতর্ক থাকুন, `std::string::length()` একটি `size_t` টাইপ ফেরত দেয়, একটি অনির্দেশিত পূর্ণসংখ্যা, যা এক্সপ্রেশনে নির্দেশিত টাইপের সাথে মিশ্রিত হলে অপ্রত্যাশিত আচরণের জন্ম দিতে পারে।

## দেখুন এছাড়াও:
- `std::string::length()` এর জন্য C++ রেফারেন্স: https://en.cppreference.com/w/cpp/string/basic_string/length
- `strlen()` এর জন্য C++ রেফারেন্স: https://en.cppreference.com/w/cpp/string/byte/strlen
- `std::string` এবং C-স্টাইল স্ট্রিং সম্পর্কে আরও: https://www.learncpp.com/cpp-tutorial/4-4a-c-style-strings/
- `std::string` ক্লাস সম্পর্কে গভীরে জানতে আগ্রহীদের জন্য: https://en.cppreference.com/w/cpp/string/basic_string

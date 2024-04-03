---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:12:46.542355-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: C++-\u098F \u0989\u09A6\u09CD\u09A7\
  \u09C3\u09A4\u09BF\u099A\u09BF\u09B9\u09CD\u09A8\u0997\u09C1\u09B2\u09BF \u09B8\u09B0\
  \u09BE\u09A8\u09CB\u09B0 \u098F\u0995\u099F\u09BF \u09B8\u09CB\u099C\u09BE\u09B8\
  \u09BE\u09AA\u099F\u09BE \u0989\u09AA\u09BE\u09AF\u09BC \u09A8\u09BF\u09AE\u09CD\
  \u09A8\u09B0\u09C2\u09AA."
lastmod: '2024-03-17T18:47:44.352460-06:00'
model: gpt-4-0125-preview
summary: "C++-\u098F \u0989\u09A6\u09CD\u09A7\u09C3\u09A4\u09BF\u099A\u09BF\u09B9\u09CD\
  \u09A8\u0997\u09C1\u09B2\u09BF \u09B8\u09B0\u09BE\u09A8\u09CB\u09B0 \u098F\u0995\
  \u099F\u09BF \u09B8\u09CB\u099C\u09BE\u09B8\u09BE\u09AA\u099F\u09BE \u0989\u09AA\
  \u09BE\u09AF\u09BC \u09A8\u09BF\u09AE\u09CD\u09A8\u09B0\u09C2\u09AA."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u0989\
  \u09A6\u09CD\u09A7\u09C3\u09A4\u09BF \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\
  \u09BE"
weight: 9
---

## কিভাবে:
C++-এ উদ্ধৃতিচিহ্নগুলি সরানোর একটি সোজাসাপটা উপায় নিম্নরূপ:

```cpp
#include <iostream>
#include <algorithm>

std::string remove_quotes(std::string input) {
    input.erase(std::remove(input.begin(), input.end(), '\"'), input.end());
    input.erase(std::remove(input.begin(), input.end(), '\''), input.end());
    return input;
}

int main() {
    std::string original = R"("Hello, 'World'!")";
    std::string no_quotes = remove_quotes(original);
    std::cout << no_quotes << std::endl;
    return 0;
}
```

এটি রান করান এবং আপনি পেয়ে যাবেন:

```
Hello, World!
```

ভয়লা! উদ্ধৃতিচিহ্নগুলি উধাও।

## গভীরে ডুব দেওয়া
উদ্ধৃতিচিহ্নগুলি কম্পিউটিং-এর সূচনা থেকেই একটি টেক্সটের সমস্যা হয়ে এসেছে। প্রাচীন সময়ে, আপনি প্রোগ্রামারদের দেখতে পাবেন যারা প্রতিটি অক্ষরের মাধ্যমে ধৈর্যশীলভাবে লুপ চালিয়ে সেই উদ্ধৃতিচিহ্নগুলি ফিল্টার আউট করে। আজ, আমাদের কাছে স্ট্যান্ডার্ড টেমপ্লেট লাইব্রেরি (STL) -এ `std::remove` রয়েছে যা ভারী কাজ করে।

বিকল্প? নিশ্চয়ই! আপনি উদ্ধৃতিচিহ্ন টার্গেট করতে `std::regex` এর সাথে নিয়মিত এক্সপ্রেশন ব্যবহার করতে পারেন, তবে এটি একটি সাধারণ কাজের জন্য স্লেজহ্যামার ব্যবহার করা মতো - শক্তিশালী, তবে সরল কাজের জন্য অতিরিক্ত হতে পারে। সম্প্রতি C++ সংস্কারনের পক্ষে, আপনি `std::string_view` এর সাথে অ-পরিবর্তনশীল পদ্ধতির জন্য নাও আগ্রহী হতে পারেন।

প্রয়োগের দিক থেকে মনে রাখা ভাল, `std::remove` আসলে কন্টেইনার থেকে উপাদান সরায় না; এটি অপসারিত না হওয়া উপাদানগুলিকে সামনে সরিয়ে নিয়ে যায় এবং পরিসীমার নতুন শেষ অংশের পরে একটি ইটারেটর ফেরত দেয়। এজন্যই আমাদের অবাঞ্ছিত শেষাংশটি কেটে ফেলার জন্য `erase` পদ্ধতির প্রয়োজন হয়।

## আরো দেখুন
- C++ `std::remove` রেফারেন্স: [cppreference.com](https://en.cppreference.com/w/cpp/algorithm/remove)
- `std::string` ম্যানিপুলেশন সম্পর্কে আরো: [cplusplus.com](http://www.cplusplus.com/reference/string/string/)

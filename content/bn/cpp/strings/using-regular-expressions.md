---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:26:09.052217-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: C++11 \u09B8\u09CD\u099F\u09CD\
  \u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u09B2\u09BE\u0987\u09AC\u09CD\
  \u09B0\u09C7\u09B0\u09BF\u09A4\u09C7 `<regex>` \u098F\u09B0 \u09AE\u09BE\u09A7\u09CD\
  \u09AF\u09AE\u09C7 \u09A8\u09BF\u09AF\u09BC\u09AE\u09BF\u09A4 \u0985\u09AD\u09BF\
  \u09AC\u09CD\u09AF\u0995\u09CD\u09A4\u09BF\u09B0 \u099C\u09A8\u09CD\u09AF \u09B8\
  \u09AE\u09B0\u09CD\u09A5\u09A8 \u099A\u09BE\u09B2\u09C1 \u0995\u09B0\u09C7\u099B\
  \u09C7, \u09AF\u09BE \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0985\u09A8\u09C1\
  \u09B8\u09A8\u09CD\u09A7\u09BE\u09A8 \u098F\u09AC\u0982 \u09AE\u09CD\u09AF\u09BE\
  \u09A8\u09BF\u09AA\u09C1\u09B2\u09C7\u09B6\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u098F\u0995\u099F\u09BF\u2026"
lastmod: '2024-03-17T18:47:44.354514-06:00'
model: gpt-4-0125-preview
summary: "C++11 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\
  \u09CD\u09A1 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u09A4\u09C7\
  \ `<regex>` \u098F\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09A8\u09BF\
  \u09AF\u09BC\u09AE\u09BF\u09A4 \u0985\u09AD\u09BF\u09AC\u09CD\u09AF\u0995\u09CD\u09A4\
  \u09BF\u09B0 \u099C\u09A8\u09CD\u09AF \u09B8\u09AE\u09B0\u09CD\u09A5\u09A8 \u099A\
  \u09BE\u09B2\u09C1 \u0995\u09B0\u09C7\u099B\u09C7, \u09AF\u09BE \u09B8\u09CD\u099F\
  \u09CD\u09B0\u09BF\u0982 \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\u09A7\u09BE\u09A8\
  \ \u098F\u09AC\u0982 \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\u09AA\u09C1\u09B2\u09C7\
  \u09B6\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF \u09A6\
  \u09C3\u09A2\u09BC \u0995\u09BE\u09A0\u09BE\u09AE\u09CB \u09AA\u09CD\u09B0\u09A6\
  \u09BE\u09A8 \u0995\u09B0\u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\
  \u099F\u09BF \u09AE\u09CC\u09B2\u09BF\u0995 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2, \u09AF\u09C7\u0996\u09BE\u09A8\
  \u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09AE\u09A7\u09CD\
  \u09AF\u09C7 \u098F\u0995\u099F\u09BF \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\
  \u09CD\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09A8\u09BF\u09AF\u09BC\u09AE\
  \u09BF\u09A4 \u0985\u09AD\u09BF\u09AC\u09CD\u09AF\u0995\u09CD\u09A4\u09BF \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u0985\u09A8\u09C1\u09B8\
  \u09A8\u09CD\u09A7\u09BE\u09A8 \u0995\u09B0\u09BE \u09B9\u09AF\u09BC."
title: "\u09B0\u09C7\u0997\u09C1\u09B2\u09BE\u09B0 \u098F\u0995\u09CD\u09B8\u09AA\u09CD\
  \u09B0\u09C7\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09BE"
weight: 11
---

## কিভাবে:
C++11 স্ট্যান্ডার্ড লাইব্রেরিতে `<regex>` এর মাধ্যমে নিয়মিত অভিব্যক্তির জন্য সমর্থন চালু করেছে, যা স্ট্রিং অনুসন্ধান এবং ম্যানিপুলেশনের জন্য একটি দৃঢ় কাঠামো প্রদান করে। এখানে একটি মৌলিক উদাহরণ দেওয়া হল, যেখানে স্ট্রিং এর মধ্যে একটি প্যাটার্নের জন্য নিয়মিত অভিব্যক্তি ব্যবহার করে অনুসন্ধান করা হয়:

```cpp
#include <iostream>
#include <regex>

int main() {
    std::string target = "Hello, my email is example@example.com";
    std::regex email_pattern(R"(\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}\b)");

    if (std::regex_search(target, email_pattern)) {
        std::cout << "ইমেইল পাওয়া গেছে!" << std::endl;
    } else {
        std::cout << "কোনো ইমেইল পাওয়া যায়নি।" << std::endl;
    }

    return 0;
}
```
**নমুনা আউটপুট**
```
ইমেইল পাওয়া গেছে!
```

স্ট্রিংগুলিতে প্যাটার্ন প্রতিস্থাপনের মতো জটিল ম্যানিপুলেশনগুলির জন্য, C++ এর নিয়মিত অভিব্যক্তিগুলি খুবই কার্যকর:

```cpp
#include <iostream>
#include <regex>

int main() {
    std::string text = "The rain in Spain falls mainly in the plain.";
    std::regex vowel_regex("([aeiou])");

    std::string replaced_text = std::regex_replace(text, vowel_regex, "*");
    std::cout << replaced_text << std::endl;

    return 0;
}
```
**নমুনা আউটপুট**
```
Th* r**n *n Sp**n f*lls m**nly *n th* pl**n.
```

স্ট্যান্ডার্ড লাইব্রেরির বাইরে প্রোগ্রামারদের অন্বেষণের জন্য, Boost Regex লাইব্রেরি (`boost/regex.hpp`) জটিল প্যাটার্নগুলি বা ব্যাপক ডেটা প্রসেসিংয়ের জন্য উন্নত রেগেক্স ক্ষমতা এবং কর্মক্ষমতা অপ্টিমাইজেশন প্রদান করে একটি জনপ্রিয় তৃতীয়-পক্ষের বিকল্প:

```cpp
#include <iostream>
#include <boost/regex.hpp>

int main() {
    std::string s = "Boost libraries are fun!";
    boost::regex expr("(\\w+)\\s(libraries)"); // "Boost libraries" মিল করে
    std::string fmt("GNU \\1"); // "GNU Boost" এর সাথে প্রতিস্থাপন করে

    std::string result = boost::regex_replace(s, expr, fmt);
    std::cout << result << std::endl;

    return 0;
}
```
**নমুনা আউটপুট**
```
GNU Boost মজাদার!
```

এই উদাহরণগুলি C++ এর সাথে নিয়মিত অভিব্যক্তির সামর্থ্যগুলির উপরিভাগে একটি স্ক্র্যাচ প্রদান করে, যা মৌলিক অনুসন্ধান, প্যাটার্ন মিল, এবং প্রতিস্থাপনগুলি নির্দেশ করে, যা হয় স্ট্যান্ডার্ড লাইব্রেরি ব্যবহার করে, অথবা Boost এর শক্তিশালী রেগেক্স বাস্তবায়ন দ্বারা উন্নত।

---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:38:48.359463-06:00
description: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u098F\
  \u0995\u099F\u09BF \u0995\u09BE\u09B2\u09CD\u09AA\u09A8\u09BF\u0995 \u098F\u0995\
  \u0995 \u09AF\u09CB\u0997 \u0995\u09B0\u09C7 \u09AC\u09BE\u09B8\u09CD\u09A4\u09AC\
  \ \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09AA\
  \u09CD\u09B0\u09B8\u09BE\u09B0\u09BF\u09A4 \u0995\u09B0\u09C7, \u09AF\u09BE 'i'\
  \ \u09B9\u09BF\u09B8\u09BE\u09AC\u09C7 \u09AA\u09CD\u09B0\u09A4\u09BF\u09A8\u09BF\
  \u09A7\u09BF\u09A4\u09CD\u09AC \u0995\u09B0\u09BE \u09B9\u09AF\u09BC, \u09AF\u09C7\
  \u0996\u09BE\u09A8\u09C7 i^2 = -1\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A4\u09BE\u09A6\u09C7\u09B0 \u09B8\u09BF\
  \u09AE\u09C1\u09B2\u09C7\u09B6\u09A8,\u2026"
lastmod: '2024-03-17T18:47:44.358934-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u098F\u0995\
  \u099F\u09BF \u0995\u09BE\u09B2\u09CD\u09AA\u09A8\u09BF\u0995 \u098F\u0995\u0995\
  \ \u09AF\u09CB\u0997 \u0995\u09B0\u09C7 \u09AC\u09BE\u09B8\u09CD\u09A4\u09AC \u09B8\
  \u0982\u0996\u09CD\u09AF\u09BE\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09AA\u09CD\u09B0\
  \u09B8\u09BE\u09B0\u09BF\u09A4 \u0995\u09B0\u09C7, \u09AF\u09BE 'i' \u09B9\u09BF\
  \u09B8\u09BE\u09AC\u09C7 \u09AA\u09CD\u09B0\u09A4\u09BF\u09A8\u09BF\u09A7\u09BF\u09A4\
  \u09CD\u09AC \u0995\u09B0\u09BE \u09B9\u09AF\u09BC, \u09AF\u09C7\u0996\u09BE\u09A8\
  \u09C7 i^2 = -1\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\
  \u09B0\u09B0\u09BE \u09A4\u09BE\u09A6\u09C7\u09B0 \u09B8\u09BF\u09AE\u09C1\u09B2\
  \u09C7\u09B6\u09A8, \u09B8\u09BF\u0997\u09A8\u09CD\u09AF\u09BE\u09B2 \u09AA\u09CD\
  \u09B0\u09B8\u09C7\u09B8\u09BF\u0982, \u098F\u09AC\u0982 \u098F\u09AE\u09A8 \u0997\
  \u09A3\u09BF\u09A4 \u09B8\u09AE\u09B8\u09CD\u09AF\u09BE \u09B8\u09AE\u09BE\u09A7\
  \u09BE\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09C7\u09A8 \u09AF\u09BE \u09A6\u09C1\u0987 \u09AE\u09BE\
  \u09A4\u09CD\u09B0\u09BE\u09AF\u09BC \u0995\u09BE\u099C \u0995\u09B0\u09BE\u09B0\
  \ \u09A6\u09BE\u09AC\u09BF \u0995\u09B0\u09C7\u0964."
title: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 14
---

## কিভাবে:
C++-এ একটি বিল্ড-ইন লাইব্রেরি `<complex>` রয়েছে যা জটিল সংখ্যাগুলির সাথে কাজ করা সহজ করে। এখানে একটি দ্রুত নজর:

```cpp
#include <iostream>
#include <complex>

int main() {
    std::complex<double> num1(2.0, 3.0); // একটি জটিল সংখ্যা তৈরি করে (2 + 3i)
    std::complex<double> num2(3.0, 4.0); // আরেকটি জটিল সংখ্যা (3 + 4i)

    // যোগ
    std::complex<double> result = num1 + num2;
    std::cout << "যোগের ফলাফল: " << result << std::endl; // (5 + 7i)

    // গুণন
    result = num1 * num2;
    std::cout << "গুণের ফলাফল: " << result << std::endl; // (-6 + 17i)

    // সংজ্ঞাহরণ
    result = std::conj(num1);
    std::cout << "num1 এর সংজ্ঞাহরণ: " << result << std::endl; // (2 - 3i)
    
    return 0;
}
```

## গভীর ডুব
জটিল সংখ্যাদের একটি সমৃদ্ধ ইতিহাস রয়েছে, প্রথমে 16শ শতাব্দীতে কিউবিক সমীকরণের সমাধানে উপস্থিত হয়। এগুলি কেবল প্রোগ্রামিং নয়, অনেক ক্ষেত্রে অপরিহার্য। কম্পিউটার বিজ্ঞানে, জটিল সংখ্যাগুলি দ্বি-মাত্রিক সংখ্যা স্থান প্রয়োজন যেমন ফাস্ট ফুরিয়ার ট্রান্সফর্ম (FFT) এর মতো অ্যালগরিদমগুলির সাহায্য করে।

C++-এর `<complex>` লাইব্রেরি মানক হলেও, অন্যান্য ভাষায় বিকল্প রয়েছে, যেমন পাইথনের `complex` ডেটা টাইপ বা জাভাস্ক্রিপ্টের গণিত লাইব্রেরি। `<complex>` লাইব্রেরি নিজেই জটিল সংখ্যাগুলির জন্য ট্রিগোনোমেট্রিক, গাণিতিক এবং লগারিদমিক অপারেশনগুলি সহ ব্যাপক কার্যকারিতা প্রদান করে।

এই সংখ্যাগুলি প্রোগ্রামিং করার সময়, অন্যতম প্রাথমিক হল ভিত্তি গণিত বুঝতে পেরে ভুল হওয়া থেকে বাঁচা এবং জটিল সংজ্ঞাহরণের মতো অপারেশন বুঝতে পারা, যা কাল্পনিক অংশের চিহ্ন পরিবর্তন করে, অথবা ইউলারের সূত্র যা জটিল প্রকাশরূপগুলিকে ট্রিগনোমেট্রিক ফাংশনগুলির সাথে সম্পর্কিত করে এসবের প্রভাব।

## আরো দেখুন
- C++ স্ট্যান্ডার্ড টেমপ্লেট লাইব্রেরি ডকুমেন্টেশন: https://en.cppreference.com/w/cpp/header/complex
- জটিল সংখ্যার গভীর গণিতীয় আলোচনা: https://mathworld.wolfram.com/ComplexNumber.html
- দৃশ্যায়নের জন্য, পাইথন লাইব্রেরি Matplotlib জটিল সংখ্যাগুলি প্লট করতে পারে: https://matplotlib.org/

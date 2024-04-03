---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:51.508862-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099A\u09B2\u09C1\u09A8 \u098F\
  \u0995\u099F\u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3 \u0995\u09BE\u099C \u09A8\
  \u09C7\u0987: \u09AC\u09C3\u09A4\u09CD\u09A4\u09C7\u09B0 \u0995\u09CD\u09B7\u09C7\
  \u09A4\u09CD\u09B0\u09AB\u09B2 \u0997\u09A3\u09A8\u09BE \u0995\u09B0\u09BE\u0964\
  \ \u098F\u0995\u0987 \u09B8\u09C2\u09A4\u09CD\u09B0 \u09AA\u09CD\u09B0\u09A4\u09BF\
  \u09AC\u09BE\u09B0 \u09B2\u09C7\u0996\u09BE\u09B0 \u09AA\u09B0\u09BF\u09AC\u09B0\
  \u09CD\u09A4\u09C7, \u0986\u09AE\u09B0\u09BE \u098F\u099F\u09BF \u098F\u0995\u099F\
  \u09BF \u09AB\u09BE\u0982\u09B6\u09A8\u09C7 \u09AC\u09A8\u09CD\u09A6\u09BF \u0995\
  \u09B0\u09BF\u0964."
lastmod: '2024-03-17T18:47:44.370886-06:00'
model: gpt-4-0125-preview
summary: "\u099A\u09B2\u09C1\u09A8 \u098F\u0995\u099F\u09BF \u09B8\u09BE\u09A7\u09BE\
  \u09B0\u09A3 \u0995\u09BE\u099C \u09A8\u09C7\u0987."
title: "\u0995\u09CB\u09A1\u0995\u09C7 \u09AB\u09BE\u0982\u09B6\u09A8\u0997\u09C1\u09B2\
  \u09BF\u09A4\u09C7 \u0986\u09AF\u09BC\u09CB\u099C\u09A8 \u0995\u09B0\u09BE"
weight: 18
---

## কিভাবে:
চলুন একটি সাধারণ কাজ নেই: বৃত্তের ক্ষেত্রফল গণনা করা। একই সূত্র প্রতিবার লেখার পরিবর্তে, আমরা এটি একটি ফাংশনে বন্দি করি।

```C++
#include <iostream>
#define PI 3.14159

double calculateCircleArea(double radius) {
    return PI * radius * radius;
}

int main() {
    double r = 5.0;
    std::cout << "বৃত্তের ব্যাসার্ধ " << r << " এর ক্ষেত্রফল হল " << calculateCircleArea(r) << std::endl;
    return 0;
}
```

নমুনা আউটপুট:
```
বৃত্তের ব্যাসার্ধ 5 এর ক্ষেত্রফল হল 78.5397
```

## গভীর ডুব
ঐতিহাসিকভাবে, প্রসিডিওর এবং ফাংশন ছিল গঠনমূলক প্রোগ্রামিং এর মেরুদণ্ড, যা 1960'এ আগের অপারেটিভ প্রোগ্রামিং ভাষায় "স্প্যাঘেটি কোড" এর সমস্যা মোকাবিলা করার জন্য সমর্থন করা হয়েছিল। ওওপি (অবজেক্ট-ওরিয়েন্টেড প্রোগ্রামিং) এর মত বিকল্প এটি আরও বেশি দূরে নিয়ে যায় এই ফাংশনগুলিকে ডেটা কাঠামোর সাথে যুক্ত করে। C++ এ, আপনার নিয়মিত ফাংশন, ক্লাস মেথড (স্ট্যাটিক মেথড সহ), ল্যাম্বডাস এবং টেমপ্লেট ফাংশন রয়েছে, প্রতিটি বিভিন্ন সুবিধা প্রদান করে। ভালোভাবে সংগঠিত ফাংশন প্রয়োগ সাধারণত ডিআরওয়াই ("ডোন্ট রিপিট ইয়োরসেল্ফ") এবং SRP (সিঙ্গল রেসপনসিবিলিটি প্রিন্সিপল) এর মতো নীতিগুলি মেনে চলা অন্তর্ভুক্ত করে, যা মানে প্রতিটি ফাংশন কেবল একটি জিনিস করে এবং তা ভালোভাবে করে।

## আরও দেখুন
C++ এ ফাংশন সম্পর্কে আরও পড়তে:
- https://en.cppreference.com/w/cpp/language/functions
- https://www.learncpp.com/cpp-tutorial/77-introduction-to-functions/

ফাংশন সম্পর্কিত ডিজাইন নীতিগুলির জন্য:
- https://en.wikipedia.org/wiki/Single-responsibility_principle
- https://en.wikipedia.org/wiki/Don%27t_repeat_yourself

ল্যাম্বডা এবং উন্নত ফাংশন ব্যবহার সম্পর্কে জানুন:
- https://www.cprogramming.com/c++11/c++11-lambda-closures.html
- https://isocpp.org/wiki/faq/cpp14-language#lambda-captures

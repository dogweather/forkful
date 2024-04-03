---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:41.306156-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u098F\u0996\u09BE\u09A8\u09C7\
  \ \u0986\u09AA\u09A8\u09BF \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 C++ \u098F \u0995\
  \u09C7\u09B8 \u09AA\u09BE\u09B0\u09CD\u09A5\u0995\u09CD\u09AF\u0997\u09C1\u09B2\u09BF\
  \u0995\u09C7 \u09AD\u09C7\u0999\u09CD\u0997\u09C7 \u09AB\u09C7\u09B2\u09AC\u09C7\
  \u09A8, \u09AC\u09A1\u09BC \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF\
  \ \u099B\u09CB\u099F \u0997\u09C1\u09B2\u09BF\u09B0 \u0995\u09BE\u099B\u09C7 \u09AE\
  \u09BE\u09A5\u09BE \u09A8\u09C1\u0987\u09AF\u09BC\u09C7 \u09A6\u09C7\u0996\u09BE\
  \u09A8\u09CB \u09B9\u09B2\u09CB."
lastmod: '2024-03-17T18:47:44.351475-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0996\u09BE\u09A8\u09C7 \u0986\u09AA\u09A8\u09BF \u0995\u09BF\u09AD\
  \u09BE\u09AC\u09C7 C++ \u098F \u0995\u09C7\u09B8 \u09AA\u09BE\u09B0\u09CD\u09A5\u0995\
  \u09CD\u09AF\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09AD\u09C7\u0999\u09CD\u0997\u09C7\
  \ \u09AB\u09C7\u09B2\u09AC\u09C7\u09A8, \u09AC\u09A1\u09BC \u0985\u0995\u09CD\u09B7\
  \u09B0\u0997\u09C1\u09B2\u09BF \u099B\u09CB\u099F \u0997\u09C1\u09B2\u09BF\u09B0\
  \ \u0995\u09BE\u099B\u09C7 \u09AE\u09BE\u09A5\u09BE \u09A8\u09C1\u0987\u09AF\u09BC\
  \u09C7 \u09A6\u09C7\u0996\u09BE\u09A8\u09CB \u09B9\u09B2\u09CB."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7 \u09B2\u09CB\u09AF\u09BC\
  \u09BE\u09B0 \u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\
  \u09B0 \u0995\u09B0\u09BE"
weight: 4
---

## কিভাবে:
এখানে আপনি কিভাবে C++ এ কেস পার্থক্যগুলিকে ভেঙ্গে ফেলবেন, বড় অক্ষরগুলি ছোট গুলির কাছে মাথা নুইয়ে দেখানো হলো:

```C++
#include <iostream>
#include <string>
#include <algorithm>

int main() {
    std::string origText = "C++ makes me Shout!";
    std::string lowerText = origText;

    std::transform(origText.begin(), origText.end(), lowerText.begin(), 
                   [](unsigned char c) { return std::tolower(c); });

    std::cout << "Original: " << origText << std::endl;
    std::cout << "Lowercase: " << lowerText << std::endl;
    
    return 0;
}
```
আউটপুট:
```
Original: C++ makes me Shout!
Lowercase: c++ makes me shout!
```

## গভীর ডুবো
দিনের শুরুতে, `std::transform` এবং ল্যাম্বডাগুলি দৃশ্যপটে প্রবেশ করার আগে, কেউ প্রতিটি অক্ষরের মধ্য দিয়ে লুপ করে এবং ম্যানুয়ালি এটিকে লোয়ার করত—এটি একটু বেশি শ্রমসাধ্য ছিল। যাইহোক, `std::transform` সঙ্গে `std::tolower` দক্ষ এবং ভুল করার সম্ভাবনা কম, যদিও, C++ জানার পর, অন্যান্য উপায়গুলি অস্তিত্ব আছে। লোকালের দিকে নজর দিন: `std::tolower`’র আচরণ ভিন্নতা দেখাতে পারে। যদি আপনার প্রজেক্টে ইউনিকোডের চাহিদা থাকে, তাহলে ICU এর মতো তৃতীয়-পক্ষের লাইব্রেরিগুলিকে দেখুন যা বিশ্ব-মঞ্চের জন্য তৈরি।

C++20 এর `std::ranges::transform` সংযোজনটিও উল্লেখ করা উচিত, যা রেঞ্জ-ভিত্তিক রূপান্তর নিয়ে আসে, সিনট্যাক্সে মসলা যোগ করছে এবং 'রেঞ্জ' দর্শনকে অনুসরণ করে যেটি বোঝার দিক থেকে আরও সহজবোধ্য এবং ত্রুটি কম হওয়ার দিকে নির্দেশ করে।

বাস্তবায়নের বিবরণের জন্য, প্রতিটি অক্ষরের একটি ASCII মান আছে, এবং লোয়ারকেস এবং আপারকেসের মধ্যে পার্থক্য ধারাবাহিক। রূপান্তরগুলি এই মানগুলি একটি নিম্নমুখী অবস্থায় নিতে দেখে—মূলত সংখ্যাতাত্ত্বিক লিম্বো খেলছে।

## আরও দেখুন
যাদের আরও অধিক জানার আগ্রহ আছে তাদের জন্য:

- `std::transform` এর জন্য C++ রেফারেন্স: https://en.cppreference.com/w/cpp/algorithm/transform
- `std::tolower` এর জন্য C++ রেফারেন্স: https://en.cppreference.com/w/cpp/string/byte/tolower
- C++20 এর `std::ranges` সম্পর্কে বিস্তারিত: https://en.cppreference.com/w/cpp/ranges

ইউনিকোড সম্পর্কে জানতে চান? ICU প্রজেক্ট চেষ্টা করে দেখুন:
- ICU প্রজেক্ট: http://site.icu-project.org/home

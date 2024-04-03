---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:00.132717-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09AC\u09B0\u09CD\u09A4\u09AE\
  \u09BE\u09A8 C++ \u098F \u098F\u0995\u099F\u09BF \u0985\u09B8\u09CD\u09A5\u09BE\u09AF\
  \u09BC\u09C0 \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u098F\u09AC\u0982\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BE\u09B0 \u0989\u09AA\
  \u09BE\u09AF\u09BC \u098F\u0996\u09BE\u09A8\u09C7."
lastmod: '2024-03-17T18:47:44.384806-06:00'
model: gpt-4-0125-preview
summary: "\u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 C++ \u098F \u098F\u0995\u099F\
  \u09BF \u0985\u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0 \u09AB\u09BE\u0987\u09B2\
  \ \u09A4\u09C8\u09B0\u09BF \u098F\u09AC\u0982 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09BE\u09B0 \u0989\u09AA\u09BE\u09AF\u09BC \u098F\u0996\u09BE\
  \u09A8\u09C7."
title: "\u098F\u0995\u099F\u09BF \u09B8\u09BE\u09AE\u09AF\u09BC\u09BF\u0995 \u09AB\
  \u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE"
weight: 21
---

## কিভাবে:
বর্তমান C++ এ একটি অস্থায়ী ফাইল তৈরি এবং ব্যবহার করার উপায় এখানে:

```C++
#include <cstdio>
#include <filesystem>
#include <iostream>

int main() {
    // filesystem লাইব্রেরি ব্যবহার করে একটি অনন্য অস্থায়ী ফাইল তৈরি
    std::filesystem::path temp_path = std::filesystem::temp_directory_path() /= std::tmpnam(nullptr);

    // অস্থায়ী ফাইল খুলুন
    std::FILE* temp_file = std::fopen(temp_path.c_str(), "w+");
    if (!temp_file) {
        std::perror("File opening failed");
        return EXIT_FAILURE;
    }

    // এতে কিছু লিখুন
    std::fputs("Hello, Temp World!\n", temp_file);

    // ফাইলটি বন্ধ করা ভুলবেন না
    std::fclose(temp_file);

    // আমাদের অস্থায়ী ফাইলের পথ আউটপুট করুন
    std::cout << "Temporary file created at: " << temp_path << std::endl;

    // পরিষ্কার করুন: অস্থায়ী ফাইল মুছে দিন
    std::filesystem::remove(temp_path);

    return EXIT_SUCCESS;
}
```

নমুনা আউটপুট (বাস্তব পথ ভিন্ন হবে):

```
Temporary file created at: /tmp/abc123
```

## গভীরে ডুব:
অস্থায়ী ফাইলগুলি রাষ্ট্র সংরক্ষণ, বড় ডেটাসেট সর্টিং, অথবা এমন আউটপুট হ্যান্ডলিং এর মতো ক্ষেত্রে উপযোগী হয় যা স্থায়ীভাবে বজায় রাখার প্রয়োজন হয় না। ঐতিহাসিকভাবে, অস্থায়ী ফাইলগুলি `/tmp` মতো সাধারণ ডিরেক্টরিতে একটি সাধারণ নামকরণ স্কিম ব্যবহার করে তৈরি করা হত, যা সংঘাতের ঝুঁকি ছিল। `<filesystem>` লাইব্রেরি ব্যবহার করে আধুনিক C++ এমন সমস্যাগুলি এড়ায়।

বিকল্পগুলির মধ্যে র‌্যাম-ভিত্তিক অস্থায়ী স্টোরেজ (যেমন অধিকাংশ Unix-এর মতো সিস্টেমে tmpfs) বা ডাটাবেজ ব্লবগুলি ব্যবহার করা অন্তর্ভুক্ত। এই পদ্ধতিগুলি ইফেমেরাল ডেটা মেমরি অথবা পরিচালিত সিস্টেমে রেখে আই/ও ওভারহেড হ্রাস করে এবং পারফরমেন্স উন্নতি করে।

বাস্তবায়নের দিক থেকে, মনে রাখবেন যে:
- ফাইল আই/ও ব্যর্থ হতে পারে, তাই ত্রুটির জন্য সর্বদা আপনার ফাইল অপারেশনগুলি চেক করুন।
- সম্পদের ফাঁস এড়াতে সর্বদা আপনার ফাইলগুলি বন্ধ করুন।
- পরিষ্কার করুন: আপনার অস্থায়ী ফাইলগুলি মুছে ফেলুন (যদিও সিস্টেম প্রায়ই তা করে, তবে এটি একটি ভাল অভ্যাস)।

## আরও দেখুন
- [C++ ফাইলসিস্টেম লাইব্রেরি](https://en.cppreference.com/w/cpp/filesystem)
- [C++ IOstreams লাইব্রেরি](https://en.cppreference.com/w/cpp/io)
- [C তে অস্থায়ী ফাইল হ্যান্ডলিং](http://www.cplusplus.com/reference/cstdio/tmpfile/)

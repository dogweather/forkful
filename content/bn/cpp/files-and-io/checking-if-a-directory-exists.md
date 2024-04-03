---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:26.168041-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u0986\u09A7\u09C1\u09A8\u09BF\
  \u0995 C++ (C++17 \u098F\u09AC\u0982 \u098F\u09B0 \u09AA\u09B0\u09C7), \u0986\u09AA\
  \u09A8\u09BF \u09AB\u09BE\u0987\u09B2\u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\
  \ \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u098F\u0995\u099F\u09BF \u09A1\u09BF\u09B0\
  \u09C7\u0995\u09CD\u099F\u09B0\u09BF \u0985\u09B8\u09CD\u09A4\u09BF\u09A4\u09CD\u09AC\
  \ \u0986\u099B\u09C7 \u0995\u09BF\u09A8\u09BE \u09A4\u09BE \u09AF\u09BE\u099A\u09BE\
  \u0987 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u098F\u099F\
  \u09BF \u09AB\u09BE\u0987\u09B2\u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u2026"
lastmod: '2024-03-17T18:47:44.379700-06:00'
model: gpt-4-0125-preview
summary: "\u0986\u09A7\u09C1\u09A8\u09BF\u0995 C++ (C++17 \u098F\u09AC\u0982 \u098F\
  \u09B0 \u09AA\u09B0\u09C7), \u0986\u09AA\u09A8\u09BF \u09AB\u09BE\u0987\u09B2\u09B8\
  \u09BF\u09B8\u09CD\u099F\u09C7\u09AE \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\
  \u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u098F\u0995\
  \u099F\u09BF \u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF \u0985\u09B8\
  \u09CD\u09A4\u09BF\u09A4\u09CD\u09AC \u0986\u099B\u09C7 \u0995\u09BF\u09A8\u09BE\
  \ \u09A4\u09BE \u09AF\u09BE\u099A\u09BE\u0987 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\
  \u09B0\u09C7\u09A8\u0964 \u098F\u099F\u09BF \u09AB\u09BE\u0987\u09B2\u09B8\u09BF\
  \u09B8\u09CD\u099F\u09C7\u09AE \u0985\u09AA\u09BE\u09B0\u09C7\u09B6\u09A8\u0997\u09C1\
  \u09B2\u09BF \u09B8\u09AE\u09CD\u09AA\u09BE\u09A6\u09A8 \u0995\u09B0\u09BE\u09B0\
  \ \u098F\u0995\u099F\u09BF \u09B8\u09B9\u099C \u0993 \u09AE\u09BE\u09A8\u0995\u09C3\
  \u09A4 \u0989\u09AA\u09BE\u09AF\u09BC \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\
  \u09B0\u09C7, \u09AF\u09BE\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u09A1\u09BF\u09B0\
  \u09C7\u0995\u09CD\u099F\u09B0\u09BF\u09B0 \u0985\u09B8\u09CD\u09A4\u09BF\u09A4\u09CD\
  \u09AC \u09AF\u09BE\u099A\u09BE\u0987 \u0995\u09B0\u09BE \u0985\u09A8\u09CD\u09A4\
  \u09B0\u09CD\u09AD\u09C1\u0995\u09CD\u09A4\u0964."
title: "\u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF \u0986\u099B\u09C7\
  \ \u0995\u09BF\u09A8\u09BE \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\
  \u09BE"
weight: 20
---

## কিভাবে:
আধুনিক C++ (C++17 এবং এর পরে), আপনি ফাইলসিস্টেম লাইব্রেরি ব্যবহার করে একটি ডিরেক্টরি অস্তিত্ব আছে কিনা তা যাচাই করতে পারেন। এটি ফাইলসিস্টেম অপারেশনগুলি সম্পাদন করার একটি সহজ ও মানকৃত উপায় প্রদান করে, যার মধ্যে ডিরেক্টরির অস্তিত্ব যাচাই করা অন্তর্ভুক্ত।

```cpp
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;

int main() {
    const fs::path dirPath = "/path/to/directory";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "ডিরেক্টরি অস্তিত্ব আছে।" << std::endl;
    } else {
        std::cout << "ডিরেক্টরি অস্তিত্ব নেই।" << std::endl;
    }

    return 0;
}
```
ডিরেক্টরি অস্তিত্ব আছে এমন নমুনা আউটপুট:
```
ডিরেক্টরি অস্তিত্ব আছে।
```

ডিরেক্টরি অস্তিত্ব নেই এমন নমুনা আউটপুট:
```
ডিরেক্টরি অস্তিত্ব নেই।
```

প্রজেক্টগুলি যা এখনো C++17 ব্যবহার করছে না বা অতিরিক্ত বৈশিষ্ট্যের জন্য, বুস্ট ফাইলসিস্টেম লাইব্রেরি একটি জনপ্রিয় তৃতীয়-পক্ষের পছন্দ যা অনুরূপ কার্যকারিতা সরবরাহ করে।

```cpp
#include <iostream>
#include <boost/filesystem.hpp>

namespace fs = boost::filesystem;

int main() {
    const fs::path dirPath = "/path/to/directory";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "ডিরেক্টরি অস্তিত্ব আছে।" << std::endl;
    } else {
        std::cout << "ডিরেক্টরি অস্তিত্ব নেই।" << std::endl;
    }

    return 0;
}
```
বুস্ট ফাইলসিস্টেম ব্যবহার করে, আউটপুট নির্দিষ্ট পথে ডিরেক্টরির অস্তিত্বের উপর নির্ভর করে C++17 ফাইলসিস্টেমের উদাহরণের অনুরূপ হবে।

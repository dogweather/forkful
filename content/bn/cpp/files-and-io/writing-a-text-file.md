---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:39:38.861489-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: C++ \u099F\u09C7\u0995\u09CD\u09B8\
  \u099F \u09AB\u09BE\u0987\u09B2\u09C7 \u09B2\u09BF\u0996\u09A8\u09C7\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u09AC\u09C7\u09B6 \u0995\u09BF\u099B\u09C1 \u0989\u09AA\u09BE\
  \u09AF\u09BC \u0985\u09AB\u09BE\u09B0 \u0995\u09B0\u09C7, \u09A4\u09AC\u09C7 \u09B8\
  \u09AC\u099A\u09C7\u09AF\u09BC\u09C7 \u09B8\u09B0\u09B2 \u09AA\u09A6\u09CD\u09A7\
  \u09A4\u09BF \u09B9\u09B2 `<fstream>` \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\
  \u09B0\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BE, \u09AF\
  \u09BE `ofstream` (\u0986\u0989\u099F\u09AA\u09C1\u099F \u09AB\u09BE\u0987\u09B2\
  \u2026"
lastmod: '2024-03-17T18:47:44.383784-06:00'
model: gpt-4-0125-preview
summary: "C++ \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2\u09C7\
  \ \u09B2\u09BF\u0996\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09C7\u09B6\
  \ \u0995\u09BF\u099B\u09C1 \u0989\u09AA\u09BE\u09AF\u09BC \u0985\u09AB\u09BE\u09B0\
  \ \u0995\u09B0\u09C7, \u09A4\u09AC\u09C7 \u09B8\u09AC\u099A\u09C7\u09AF\u09BC\u09C7\
  \ \u09B8\u09B0\u09B2 \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF \u09B9\u09B2 `<fstream>`\
  \ \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09BE, \u09AF\u09BE `ofstream` (\u0986\u0989\u099F\
  \u09AA\u09C1\u099F \u09AB\u09BE\u0987\u09B2 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u09AE) \u0995\u09CD\u09B2\u09BE\u09B8 \u09B8\u09B0\u09AC\u09B0\u09BE\u09B9 \u0995\
  \u09B0\u09C7 \u09AF\u09BE \u09AA\u09CD\u09B0\u09BE\u09A5\u09AE\u09BF\u0995\u09AD\
  \u09BE\u09AC\u09C7 \u09AB\u09BE\u0987\u09B2 \u09B2\u09C7\u0996\u09BE\u09B0 \u0995\
  \u09BE\u09B0\u09CD\u09AF\u0995\u09CD\u09B0\u09AE\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u09A8\u09BF\u09B0\u09CD\u09AE\u09BF\u09A4\u0964\n"
title: "\u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\
  \u0987\u09B2 \u09B2\u09BF\u0996\u09BE"
weight: 24
---

## কিভাবে:
C++ টেক্সট ফাইলে লিখনের জন্য বেশ কিছু উপায় অফার করে, তবে সবচেয়ে সরল পদ্ধতি হল `<fstream>` লাইব্রেরি ব্যবহার করা, যা `ofstream` (আউটপুট ফাইল স্ট্রিম) ক্লাস সরবরাহ করে যা প্রাথমিকভাবে ফাইল লেখার কার্যক্রমের জন্য নির্মিত।

### `<fstream>` ব্যবহার করে উদাহরণ:
```cpp
#include <fstream>
#include <iostream>

int main() {
    std::ofstream file("example.txt");
    if (file.is_open()) {
        file << "Hello, world!\n";
        file << "Writing to a file in C++ is simple.";
        file.close();
    } else {
        std::cerr << "Failed to open file\n";
    }
    return 0;
}
```

**'example.txt' এ নমুনা আউটপুট:**
```
Hello, world!
Writing to a file in C++ is simple.
```

যখন আরও জটিল ডেটা নিয়ে কাজ করা হয় বা লেখার প্রক্রিয়ার উপর আরও নিয়ন্ত্রণ প্রয়োজন হয়, প্রোগ্রামাররা বুস্ট ফাইলসিস্টেম এর মতো তৃতীয় পক্ষের লাইব্রেরিদের কাছে যেতে পারেন।

### বুস্ট ফাইলসিস্টেম ব্যবহার করে উদাহরণ:
ফাইল অপারেশনের জন্য বুস্ট ব্যবহার করতে, আপনাকে প্রথমে বুস্ট লাইব্রেরিগুলি ইনস্টল করতে হবে। নিম্নলিখিত উদাহরণ `boost::filesystem` এবং `boost::iostreams` ব্যবহার করে একটি ফাইল তৈরি এবং তাতে লেখার প্রক্রিয়া প্রদর্শন করে।

```cpp
#include <boost/filesystem.hpp>
#include <boost/iostreams/device/file.hpp>
#include <boost/iostreams/stream.hpp>
#include <iostream>

namespace io = boost::iostreams;
namespace fs = boost::filesystem;

int main() {
    fs::path filePath("boost_example.txt");
    io::stream_buffer<io::file_sink> buf(filePath.string());
    std::ostream out(&buf);
    out << "Boost makes file operations easy.\n";
    out << "This is a line written with Boost.";
    
    return 0;
}
```

**'boost_example.txt' এ নমুনা আউটপুট:**
```
Boost makes file operations easy.
This is a line written with Boost.
```

আপনার প্রকল্পের নির্দিষ্ট প্রয়োজনীয়তা এবং ফাইল I/O অপারেশন্সের উপর আপনার প্রয়োজনীয় নিয়ন্ত্রণ বা নমনীয়তা নির্ভর করে, বুস্টের মতো তৃতীয় পক্ষের লাইব্রেরি এবং মৌলিক C++ এর মধ্যে পছন্দের পার্থক্য হতে পারে।

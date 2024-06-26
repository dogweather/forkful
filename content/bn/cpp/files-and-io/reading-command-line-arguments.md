---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:09:38.822933-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: C++-\u098F, \u0995\u09AE\u09BE\
  \u09A8\u09CD\u09A1-\u09B2\u09BE\u0987\u09A8 \u0986\u09B0\u09CD\u0997\u09C1\u09AE\
  \u09C7\u09A8\u09CD\u099F\u0997\u09C1\u09B2\u09BF `main()`-\u098F \u0995\u09CD\u09AF\
  \u09BE\u09B0\u09C7\u0995\u09CD\u099F\u09BE\u09B0 \u09AA\u09AF\u09BC\u09C7\u09A8\u09CD\
  \u099F\u09BE\u09B0\u09B8\u09C7\u09B0 \u098F\u0995\u099F\u09BF \u0985\u09CD\u09AF\
  \u09BE\u09B0\u09C7 \u09B9\u09BF\u09B8\u09C7\u09AC\u09C7 \u09AA\u09BE\u0993\u09AF\
  \u09BC\u09BE \u09AF\u09BE\u09AF\u09BC\u0964 \u098F\u0997\u09C1\u09B2\u09BF \u0995\
  \u09BF\u09AD\u09BE\u09AC\u09C7 \u09A7\u09B0\u09A4\u09C7 \u09B9\u09AC\u09C7 \u09A4\
  \u09BE \u09A8\u09BF\u099A\u09C7 \u09A6\u09C7\u0996\u09BE\u09A8\u09CB \u09B9\u09B2\
  \u09CB."
lastmod: '2024-03-17T18:47:44.380716-06:00'
model: gpt-4-0125-preview
summary: "C++-\u098F, \u0995\u09AE\u09BE\u09A8\u09CD\u09A1-\u09B2\u09BE\u0987\u09A8\
  \ \u0986\u09B0\u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F\u0997\u09C1\u09B2\
  \u09BF `main()`-\u098F \u0995\u09CD\u09AF\u09BE\u09B0\u09C7\u0995\u09CD\u099F\u09BE\
  \u09B0 \u09AA\u09AF\u09BC\u09C7\u09A8\u09CD\u099F\u09BE\u09B0\u09B8\u09C7\u09B0\
  \ \u098F\u0995\u099F\u09BF \u0985\u09CD\u09AF\u09BE\u09B0\u09C7 \u09B9\u09BF\u09B8\
  \u09C7\u09AC\u09C7 \u09AA\u09BE\u0993\u09AF\u09BC\u09BE \u09AF\u09BE\u09AF\u09BC\
  \u0964 \u098F\u0997\u09C1\u09B2\u09BF \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u09A7\
  \u09B0\u09A4\u09C7 \u09B9\u09AC\u09C7 \u09A4\u09BE \u09A8\u09BF\u099A\u09C7 \u09A6\
  \u09C7\u0996\u09BE\u09A8\u09CB \u09B9\u09B2\u09CB."
title: "\u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\u09B0\
  \u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F\u0997\u09C1\u09B2\u09BF \u09AA\u09A1\
  \u09BC\u09BE"
weight: 23
---

## কিভাবে:
C++-এ, কমান্ড-লাইন আর্গুমেন্টগুলি `main()`-এ ক্যারেক্টার পয়েন্টারসের একটি অ্যারে হিসেবে পাওয়া যায়। এগুলি কিভাবে ধরতে হবে তা নিচে দেখানো হলো:

```C++
#include <iostream>
int main(int argc, char* argv[]) {
    std::cout << "আপনি " << argc << " আর্গুমেন্ট এন্টার করেছেন:\n";
    for (int i = 0; i < argc; ++i) {
        std::cout << argv[i] << "\n";
    }
    return 0;
}
```

উদাহরণ আউটপুট: (ধরে নেওয়া হলো যে, এটি `./myProgram foo bar` হিসাবে চালানো হয়েছে)

```plaintext
আপনি 3 আর্গুমেন্ট এন্টার করেছেন:
./myProgram
foo
bar
```

## গভীরে যাওয়া
অনেক আগে, কমান্ড লাইনই ছিল প্রোগ্রামের সাথে মিথস্ক্রিয়া করার একমাত্র উপায়। আজকের GUI-গুলি অসাধারণ, কিন্তু কমান্ড লাইন এখনও টিকে আছে, বিশেষ করে সার্ভার অথবা ডেভেলপমেন্ট পরিবেশে। এটি দ্রুত, স্ক্রিপ্টযোগ্য নিয়ন্ত্রণ অফার করে।

`argv` এবং `argc` এর জন্য অল্টারনেটিভ লাইব্রেরিগুলির মধ্যে আছে `Boost.Program_options`, যা আরও ফ্যান্সি পারসিং এর জন্য। ইউনিক্স-মত সিস্টেমগুলিতে ট্র্যাডিশনাল কমান্ড লাইন ফ্যানের জন্য আরও একটি ফাংশন হল `getopt()`।

স্ক্র্যাচ থেকে আর্গুমেন্ট পার্সিং ইমপ্লিমেন্ট করা আপনাকে তা কাস্টমাইজ করতে দেয়, কিন্তু সিকিউরিটি হোলগুলির জন্য দেখাশোনা করবেন। ব্যবহারকারীর ইনপুটে অন্ধভাবে বিশ্বাস করবেন না—সর্বদা ভ্যালিডেট এবং স্যানিটাইজ করুন।

## দেখা জরুরি
- `main()` ফাংশন সম্পর্কিত C++ ডকুমেন্টেশন: https://en.cppreference.com/w/cpp/language/main_function
- Boost.Program_options: https://www.boost.org/doc/libs/release/libs/program_options/
- GNU `getopt()` টিউটোরিয়াল: https://www.gnu.org/software/libc/manual/html_node/Getopt.html

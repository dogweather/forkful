---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:11:33.386309-06:00
description: "\u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0\u09BF\u0982\
  \ \u09B9\u09B2\u09CB \u098F\u0995\u099F\u09BF \u0995\u09AE\u09CD\u09AA\u09BF\u0989\
  \u099F\u09BE\u09B0 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09C7\u09B0\
  \ \u0985\u09AD\u09CD\u09AF\u09A8\u09CD\u09A4\u09B0\u09C0\u09A3 \u0995\u09BE\u09A0\
  \u09BE\u09AE\u09CB \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\
  \u09BE\u09B0 \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE, \u09AF\
  \u09BE \u09A4\u09BE\u09B0 \u09AC\u09BE\u09B9\u09CD\u09AF\u09BF\u0995 \u0986\u099A\
  \u09B0\u09A3 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u09A8\u09BE \u0995\
  \u09B0\u09C7\u0987 \u0995\u09B0\u09BE \u09B9\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A4\u09BE\u09A6\u09C7\
  \u09B0\u2026"
lastmod: '2024-03-17T18:47:44.373800-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0\u09BF\u0982\
  \ \u09B9\u09B2\u09CB \u098F\u0995\u099F\u09BF \u0995\u09AE\u09CD\u09AA\u09BF\u0989\
  \u099F\u09BE\u09B0 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09C7\u09B0\
  \ \u0985\u09AD\u09CD\u09AF\u09A8\u09CD\u09A4\u09B0\u09C0\u09A3 \u0995\u09BE\u09A0\
  \u09BE\u09AE\u09CB \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\
  \u09BE\u09B0 \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE, \u09AF\
  \u09BE \u09A4\u09BE\u09B0 \u09AC\u09BE\u09B9\u09CD\u09AF\u09BF\u0995 \u0986\u099A\
  \u09B0\u09A3 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u09A8\u09BE \u0995\
  \u09B0\u09C7\u0987 \u0995\u09B0\u09BE \u09B9\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A4\u09BE\u09A6\u09C7\
  \u09B0\u2026"
title: "\u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0\u09BF\u0982"
---

{{< edit_this_page >}}

## কি এবং কেন?

রিফ্যাক্টরিং হলো একটি কম্পিউটার প্রোগ্রামের অভ্যন্তরীণ কাঠামো পরিবর্তন করার প্রক্রিয়া, যা তার বাহ্যিক আচরণ পরিবর্তন না করেই করা হয়। প্রোগ্রামাররা তাদের কোডকে পরিষ্কার করার জন্য, যা তা বোঝা, রক্ষণাবেক্ষণ এবং বিস্তারিত করা সহজ করে তোলে, এ জন্য তারা এটি করে থাকেন।

## কিভাবে:

কল্পনা করুন আপনার কাছে এমন একটি ফাংশন আছে যা একটু বেশি কাজ করে, যেমন এই ভারী মেথডটি যা একটি অবজেক্ট ইনিশিয়ালাইজ করে এবং লগিংও সম্পাদন করে:

```C++
#include <iostream>

class Widget {
public:
    void init(bool verbose) {
        // ইনিশিয়ালাইজেশন লজিক
        // ...

        // ভার্বোজ লগিং
        if (verbose) {
            std::cout << "Widget initialized!" << std::endl;
        }
    }
};

// ব্যবহার:
Widget w;
w.init(true);
```

আউটপুট:
```
Widget initialized!
```

এটি আরও পরিষ্কার, আরও ফোকাসড মেথডে রিফ্যাক্টরিং করলে এরকম দেখাবে:

```C++
#include <iostream>

class Widget {
public:
    void init() {
        // শুধুমাত্র ইনিশিয়ালাইজেশন লজিক
        // ...
    }

    void logInitialization() const {
        std::cout << "Widget initialized!" << std::endl;
    }
};

// ব্যবহার:
Widget w;
w.init();
w.logInitialization();
```

এই পরিবর্তন প্রোগ্রামটি কি করে তা পরিবর্তন না করেও `Widget` ক্লাসকে আরও মডিউলার এবং এর ব্যবহারকে আরও স্পষ্ট করে তুলেছে।

## গভীর ডুব

যেমন আমরা জানি আজকের রিফ্যাক্টরিং ধারণাটির শেকড় রয়েছে ১৯৮০-এর দশকের স্মলটক প্রোগ্রামিং কমিউনিটিতে এবং এটি মার্টিন ফাউলারের "রিফ্যাক্টরিং: ইম্প্রুভিং দ্য ডিজাইন অফ এক্সিস্টিং কোড" বই দ্বারা প্রচণ্ডভাবে জনপ্রিয় হয়েছিল যা ১৯৯৯ সালে প্রকাশিত হয়েছিল। আজকে, রিফ্যাক্টরিং আধুনিক সফটওয়্যার ডেভেলপমেন্টের একটি মূল অংশ, যা এজাইল এবং TDD (টেস্ট-ড্রাইভেন ডেভেলপমেন্ট) এর মতো বিভিন্ন ডেভেলপমেন্ট পদ্ধতিতে একীভূত করা হয়েছে।

রিফ্যাক্টরিংয়ের বিকল্প সম্পর্কে আলোচনা করলে, আমরা পুনর্লিখন বা পুনঃডিজাইনের অঞ্চলে চলে যাই। রিফ্যাক্টরিং কৌশলগত এবং ক্রমান্বয়ী, অন্যদিকে একটি পুনর্লিখন বিদ্যমান কোডকে ফেলে দিয়ে নতুন সমাধানের পক্ষে যেতে পারে। পুনঃডিজাইন হলো, এদিকে, আরও গুরুতর পরিবর্তনসমূহ অন্তর্ভুক্ত করতে পারে যা কার্যকারিতা পরিবর্তন করতে পারে, যা শুদ্ধ রিফ্যাক্টরিংয়ের জন্য একটি অ-লক্ষ্য।

রিফ্যাক্টরিং সম্পর্কে বাস্তবায়নের বিস্তারিত তথ্য বেশ সূক্ষ্ম হতে পারে। রিফ্যাক্টরিং উদ্বুদ্ধ করতে পারে এমন অনেক 'কোড স্মেল' আছে, যেমন দীর্ঘ মেথড, বড় ক্লাস, অথবা ডুপ্লিকেট কোড। "Clang-Tidy" এর মতো স্বয়ংক্রিয় টুলস আছে যা সি++, এ সমস্যাগুলি চিহ্নিত করতে এবং কিছু সংশোধন প্রয়োগ করতে সাহায্য করতে পারে।

তদুপরি, রিফ্যাক্টরিং একটি দৃঢ় টেস্ট স্যুট প্রয়োজন করে যাতে করে কার্যকারিতা অপরিবর্তিত থাকে। টেস্ট ছাড়া, আপনি মূলত অন্ধকারে উড়ছেন এবং রিগ্রেশনের ঝুঁকি নিচ্ছেন।

## আরও দেখুন

রিফ্যাক্টরিং সম্পর্কে গভীর ধারণা পেতে এবং আরও উদাহরণ দেখতে আপনার যেতে পারেন:
- মার্টিন ফাউলারের ক্লাসিক টেক্সট "রিফ্যাক্টরিং: ইম্প্রুভিং দ্য ডিজাইন অফ এক্সিস্টিং কোড" মৌলিক ধারণা ও কৌশলগুলি জন্য।
- সি++ এ স্বয়ংক্রিয় রিফ্যাক্টরিং সাপোর্টের জন্য `Clang-Tidy` ডকুমেন্টেশন https://clang.llvm.org/extra/clang-tidy/ দেখুন।
- মাইকেল ফেদার্সের "ওয়ার্কিং এফেক্টিভলি উইথ লেগ্যাসি কোড" যা কম নিখুঁত বিদ্যমান কোডবেসগুলিতে নিরাপদে রিফ্যাক্টরিং করার কৌশল প্রদান করে।
---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:11:46.682287-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B0\u09BF\u09AB\u09CD\u09AF\
  \u09BE\u0995\u09CD\u099F\u09B0\u09BF\u0982 \u098F\u09B0 \u09AA\u09C2\u09B0\u09CD\
  \u09AC\u09C7, \u0986\u09AA\u09A8\u09BF \u098F\u09AE\u09A8 \u098F\u0995\u099F\u09BF\
  \ \u0995\u09CB\u09A1\u09C7\u09B0 \u099F\u09C1\u0995\u09B0\u09CB \u09A5\u09BE\u0995\
  \u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7 \u09AF\u09BE \u09AC\u09BF\u09AD\u09BF\u09A8\
  \u09CD\u09A8 \u09B8\u09CD\u09A4\u09B0\u09C7\u09B0 \u0986\u09AC\u09B8\u09CD\u099F\
  \u09CD\u09B0\u09BE\u0995\u09B6\u09A8 \u09AC\u09BE \u09A6\u09BE\u09AF\u09BC\u09BF\
  \u09A4\u09CD\u09AC\u0995\u09C7 \u09AE\u09BF\u09B6\u09CD\u09B0\u09BF\u09A4 \u0995\
  \u09B0\u09C7, \u09AF\u09C7\u09AE\u09A8 \u099B\u09BE\u09A1\u09BC \u09B9\u09BF\u09B8\
  \u09BE\u09AC \u0995\u09B0\u09BE \u098F\u09AC\u0982\u2026"
lastmod: '2024-04-05T21:53:51.843284-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0\u09BF\u0982\
  \ \u098F\u09B0 \u09AA\u09C2\u09B0\u09CD\u09AC\u09C7, \u0986\u09AA\u09A8\u09BF \u098F\
  \u09AE\u09A8 \u098F\u0995\u099F\u09BF \u0995\u09CB\u09A1\u09C7\u09B0 \u099F\u09C1\
  \u0995\u09B0\u09CB \u09A5\u09BE\u0995\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7 \u09AF\
  \u09BE \u09AC\u09BF\u09AD\u09BF\u09A8\u09CD\u09A8 \u09B8\u09CD\u09A4\u09B0\u09C7\
  \u09B0 \u0986\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BE\u0995\u09B6\u09A8 \u09AC\
  \u09BE \u09A6\u09BE\u09AF\u09BC\u09BF\u09A4\u09CD\u09AC\u0995\u09C7 \u09AE\u09BF\
  \u09B6\u09CD\u09B0\u09BF\u09A4 \u0995\u09B0\u09C7, \u09AF\u09C7\u09AE\u09A8 \u099B\
  \u09BE\u09A1\u09BC \u09B9\u09BF\u09B8\u09BE\u09AC \u0995\u09B0\u09BE \u098F\u09AC\
  \u0982 \u09A4\u09BE\u09B0\u09AA\u09B0 \u09A4\u09BE \u09AA\u09CD\u09B0\u09AF\u09BC\
  \u09CB\u0997 \u0995\u09B0\u09BE."
title: "\u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0\u09BF\u0982"
weight: 19
---

## কিভাবে:


### উদাহরণ ১: নাম পরিবর্তন এবং মেথড প্রত্যাহার
রিফ্যাক্টরিং এর পূর্বে, আপনি এমন একটি কোডের টুকরো থাকতে পারে যা বিভিন্ন স্তরের আবস্ট্রাকশন বা দায়িত্বকে মিশ্রিত করে, যেমন ছাড় হিসাব করা এবং তারপর তা প্রয়োগ করা:

```dart
void main() {
  var price = 100.0;
  var discount = 0.2;
  var finalPrice = price - (price * discount);
  print("চূড়ান্ত দাম: $finalPrice");
}
```

**আউটপুট:**
```
চূড়ান্ত দাম: 80.0
```

রিফ্যাক্টরিং এর পরে, আপনি ছাড় হিসাবের কাজটি একটি নিজস্ব মেথডে প্রত্যাহার করে এবং এর একটি অর্থপূর্ণ নাম দিতে পারেন:

```dart
void main() {
  var price = 100.0;
  var discount = 0.2;
  var finalPrice = calculateFinalPrice(price, discount);
  print("চূড়ান্ত দাম: $finalPrice");
}

double calculateFinalPrice(double price, double discount) {
  return price - (price * discount);
}
```

**আউটপুট:**
```
চূড়ান্ত দাম: 80.0
```

হিসাবের কাজটি একটি মেথডে প্রত্যাহার করে, আপনি এখন একটি পরিষ্কারভাবে নির্ধারিত অপারেশন পেয়েছেন যা পুনঃব্যবহার, স্বাধীনভাবে পরীক্ষিত, এবং সহজেই সংশোধিত করা যেতে পারে।

### উদাহরণ ২: শর্তনাম সহজীকরণ
রিফ্যাক্টরিং এর আগে, শর্তনামগুলি অত্যধিক জটিল বা পড়তে কঠিন হতে পারে:

```dart
void main() {
  var customerType = "regular";
  double discount;
  
  if (customerType == "regular") {
    discount = 0.05;
  } else if (customerType == "member") {
    discount = 0.1;
  } else {
    discount = 0.0;
  }

  print("ছাড়: $discount");
}
```

**আউটপুট:**
```
ছাড়: 0.05
```

রিফ্যাক্টরিং এর পরে, গ্রাহকের ধরন এবং ছাড়ের সহজ আপডেট বা বিস্তারের জন্য একটি ম্যাপ ব্যবহার করার বিবেচনা করুন:

```dart
void main() {
  var customerType = "regular";
  var discounts = {
    "regular": 0.05,
    "member": 0.1,
    "none": 0.0,
  };

  var discount = discounts[customerType] ?? 0.0;
  print("ছাড়: $discount");
}
```

**আউটপুট:**
```
ছাড়: 0.05
```

এই রিফ্যাক্টরটি না শুধুমাত্র কোডকে আরও সংক্ষিপ্ত করে তোলে, কিন্তু ছাড় নির্ধারণের যৌক্তিকতাকে এমনভাবে এনক্যাপসুলেট করে যা বুঝতে এবং বজায় রাখার জন্য সহজ।

### রিফ্যাক্টরিং এর জন্য তৃতীয় পক্ষের লাইব্রেরি
Dart-এ, বিশেষ করে Flutter অ্যাপ্লিকেশনের মধ্যে রিফ্যাক্টরিং করার ক্ষেত্রে, [Dart DevTools](https://dart.dev/tools/dart-devtools) সুইট অমূল্য। এটি পারফরমেন্স টুল, একটি উইজেট ইন্সপেক্টর, এবং একটি সোর্স-লেভেল ডিবাগার অন্তর্ভুক্ত করে। যদিও এটি কোনো তৃতীয় পক্ষের লাইব্রেরি নয়, Dart DevTools প্রায়শই `flutter_bloc` এর মতো লাইব্রেরিগুলির সাথে ব্যবহৃত হয়, যা রিফ্যাক্টরিং এর জন্য উন্নত মডিউলারিটি এবং পড়ার সহজতার সাথে স্টেট পরিচালনা করে। দুর্ভাগ্যবশত, এই প্রবেশের পরিধির কারণে, তৃতীয় পক্ষের লাইব্রেরি ব্যবহার করে নির্দিষ্ট কোডের উদাহরণ এখানে প্রদান করা হবে না, কিন্তু ডেভেলপারদের Dart/Flutter অ্যাপ্লিকেশনগুলিতে রিফ্যাক্টরিং প্রক্রিয়া উন্নত করার জন্য এই টুলগুলি অন্বেষণ করার উৎসাহিত করা হচ্ছে।

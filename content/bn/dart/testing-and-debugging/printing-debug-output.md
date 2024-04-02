---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:07:09.753130-06:00
description: "\u09A1\u09BE\u09B0\u09CD\u099F\u09C7 \u09A1\u09BF\u09AC\u09BE\u0997\
  \ \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AA\u09CD\u09B0\u09BF\u09A8\u09CD\u099F\
  \ \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B0\u09BE\u09A8\u099F\u09BE\u0987\
  \u09AE\u09C7 \u0995\u09A8\u09B8\u09CB\u09B2\u09C7 \u09A4\u09A5\u09CD\u09AF \u09AA\
  \u09CD\u09B0\u09A6\u09B0\u09CD\u09B6\u09A8 \u0995\u09B0\u09BE, \u09AF\u09BE \u09A1\
  \u09C7\u09AD\u09C7\u09B2\u09AA\u09BE\u09B0\u09A6\u09C7\u09B0 \u0995\u09BE\u09B0\u09CD\
  \u09AF\u09AA\u09CD\u09B0\u09AC\u09BE\u09B9 \u0985\u09A8\u09C1\u09B8\u09B0\u09A3\
  \ \u0995\u09B0\u09A4\u09C7, \u09AD\u09C7\u09B0\u09BF\u09AF\u09BC\u09C7\u09AC\u09B2\
  \u0997\u09C1\u09B2\u09BF\u09B0 \u0985\u09AC\u09B8\u09CD\u09A5\u09BE \u09A4\u09A6\
  \u09A8\u09CD\u09A4 \u0995\u09B0\u09A4\u09C7, \u0985\u09A5\u09AC\u09BE\u2026"
lastmod: '2024-03-17T18:47:43.721570-06:00'
model: gpt-4-0125-preview
summary: "\u09A1\u09BE\u09B0\u09CD\u099F\u09C7 \u09A1\u09BF\u09AC\u09BE\u0997 \u0986\
  \u0989\u099F\u09AA\u09C1\u099F \u09AA\u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\
  \u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B0\u09BE\u09A8\u099F\u09BE\u0987\u09AE\
  \u09C7 \u0995\u09A8\u09B8\u09CB\u09B2\u09C7 \u09A4\u09A5\u09CD\u09AF \u09AA\u09CD\
  \u09B0\u09A6\u09B0\u09CD\u09B6\u09A8 \u0995\u09B0\u09BE, \u09AF\u09BE \u09A1\u09C7\
  \u09AD\u09C7\u09B2\u09AA\u09BE\u09B0\u09A6\u09C7\u09B0 \u0995\u09BE\u09B0\u09CD\u09AF\
  \u09AA\u09CD\u09B0\u09AC\u09BE\u09B9 \u0985\u09A8\u09C1\u09B8\u09B0\u09A3 \u0995\
  \u09B0\u09A4\u09C7, \u09AD\u09C7\u09B0\u09BF\u09AF\u09BC\u09C7\u09AC\u09B2\u0997\
  \u09C1\u09B2\u09BF\u09B0 \u0985\u09AC\u09B8\u09CD\u09A5\u09BE \u09A4\u09A6\u09A8\
  \u09CD\u09A4 \u0995\u09B0\u09A4\u09C7, \u0985\u09A5\u09AC\u09BE\u2026"
title: "\u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AA\
  \u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09BE"
weight: 33
---

## কি এবং কেন?

ডার্টে ডিবাগ আউটপুট প্রিন্ট করা মানে রানটাইমে কনসোলে তথ্য প্রদর্শন করা, যা ডেভেলপারদের কার্যপ্রবাহ অনুসরণ করতে, ভেরিয়েবলগুলির অবস্থা তদন্ত করতে, অথবা ত্রুটির উৎস চিহ্নিত করতে সহায়তা করে। ত্রুটিনির্ণয় এবং নিশ্চিত করে যে তাদের কোড প্রত্যাশিত অনুযায়ী আচরণ করে, এটি ডেভেলপারদের জন্য সাধারণভাবে ব্যবহার করা হয়, এটি একটি নিখুঁত এবং আরো কার্যকরী ডেভেলপমেন্ট প্রক্রিয়া সহজতর করে।

## কিভাবে:

ডার্টে, আপনি `print()` ফাংশন ব্যবহার করে ডিবাগ আউটপুট প্রিন্ট করতে পারেন। এখানে কীভাবে সহজ বার্তা এবং ভেরিয়েবল মান আউটপুট করা যায়:

```dart
void main() {
  String greeting = "Hello, Dart!";
  print(greeting); // প্রিন্ট করে: Hello, Dart!

  int number = 42;
  print('The number is $number.'); // প্রিন্ট করে: The number is 42.
}
```

তালিকা বা অবজেক্টের মতো গঠনযুক্ত ডেটার জন্য, ডার্টের `toString()` পদ্ধতি পর্যাপ্ত বিস্তার সরবরাহ করতে নাও পারে। এই ধরনের ক্ষেত্রে, আপনি ডাটাকে জেসন স্ট্রিংয়ে রূপান্তরিত করার জন্য ডার্টের `dart:convert` লাইব্রেরি থেকে `jsonEncode` ফাংশন ব্যবহার করতে পারেন যাতে পড়ার জন্য বিস্তারিত আউটপুট পাওয়া যায়:

```dart
import 'dart:convert';

void main() {
  var user = {
    'name': 'John Doe',
    'age': 30,
    'emails': ['john.doe@example.com', 'john@example.com'],
  };

  print(jsonEncode(user));
  // প্রিন্ট করে: {"name":"John Doe","age":30,"emails":["john.doe@example.com","john@example.com"]}
}
```

যখন আরো উন্নত ডিবাগিং ক্ষমতা প্রয়োজন হয়, যেমন আলাদা আলাদা গুরুত্বের সাথে লগিং (তথ্য, সতর্কতা, ত্রুটি), আপনি `logger` এর মতো তৃতীয় পক্ষের লাইব্রেরিগুলি ব্যবহার করতে পারেন। এটি কিভাবে ব্যবহার করবেন:

১. আপনার `pubspec.yaml` এ `logger` যোগ করুন:

```yaml
dependencies:
  logger: ^1.0.0
```

২. আপনার ডার্ট কোডে `logger` ব্যবহার করুন:

```dart
import 'package:logger/logger.dart';

var logger = Logger();

void main() {
  logger.d("This is a debug message");
  logger.w("This is a warning message");
  logger.e("This is an error message");
}
```

আউটপুট আরো তথ্যপূর্ণ হবে, বার্তার স্তর এবং বার্তাটি নিজেই দেখাবে, যা বিভিন্ন ধরনের লগ বার্তা আলাদা করে চিহ্নিত করা সহজ করে দেয়।

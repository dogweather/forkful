---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:51.850748-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09A1\u09BE\u09B0\u09CD\u099F\
  \u09C7, \u0986\u09AA\u09A8\u09BF `DateTime` \u0995\u09CD\u09B2\u09BE\u09B8 \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09A4\u09BE\u09B0\u09BF\
  \u0996 \u09A4\u09C1\u09B2\u09A8\u09BE \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\
  \u09C7\u09A8, \u09AF\u09BE `isBefore`, `isAfter`, \u098F\u09AC\u0982 `isAtSameMomentAs`\
  \ \u098F\u09B0 \u09AE\u09A4\u09CB \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF\u0997\u09C1\
  \u09B2\u09BF \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF \u09A4\u09C1\u09B2\u09A8\u09BE\
  \u09B0\u2026"
lastmod: '2024-03-17T18:47:43.732731-06:00'
model: gpt-4-0125-preview
summary: "\u09A1\u09BE\u09B0\u09CD\u099F\u09C7, \u0986\u09AA\u09A8\u09BF `DateTime`\
  \ \u0995\u09CD\u09B2\u09BE\u09B8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996 \u09A4\u09C1\u09B2\u09A8\u09BE \u0995\
  \u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8, \u09AF\u09BE `isBefore`, `isAfter`,\
  \ \u098F\u09AC\u0982 `isAtSameMomentAs` \u098F\u09B0 \u09AE\u09A4\u09CB \u09AA\u09A6\
  \u09CD\u09A7\u09A4\u09BF\u0997\u09C1\u09B2\u09BF \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF\
  \ \u09A4\u09C1\u09B2\u09A8\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09B8\u09B0\u09AC\
  \u09B0\u09BE\u09B9 \u0995\u09B0\u09C7\u0964 \u0985\u09A4\u09BF\u09B0\u09BF\u0995\
  \u09CD\u09A4\u09AD\u09BE\u09AC\u09C7, \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\u09B0\
  \ \u09AE\u09A7\u09CD\u09AF\u09C7 \u09AA\u09BE\u09B0\u09CD\u09A5\u0995\u09CD\u09AF\
  \ `difference()` \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09C7 \u09A8\u09BF\u09B0\u09CD\u09A7\u09BE\u09B0\u09A3\
  \ \u0995\u09B0\u09BE \u09AF\u09BE\u09AF\u09BC, \u09AF\u09BE \u098F\u0995\u099F\u09BF\
  \ `Duration` \u0985\u09AC\u099C\u09C7\u0995\u09CD\u099F \u09B8\u09B0\u09AC\u09B0\
  \u09BE\u09B9 \u0995\u09B0\u09C7 \u09AF\u09BE \u09A6\u09C1\u099F\u09BF \u09B8\u09AE\
  \u09AF\u09BC \u09AC\u09BF\u09A8\u09CD\u09A6\u09C1\u09B0 \u09AE\u09A7\u09CD\u09AF\
  \u09C7 \u09AC\u09CD\u09AF\u09AC\u09A7\u09BE\u09A8 \u09AC\u09BF\u09B8\u09CD\u09A4\
  \u09BE\u09B0\u09BF\u09A4 \u0995\u09B0\u09C7\u0964\n\n\u09A8\u09BF\u09AE\u09CD\u09A8\
  \u09C7 \u098F\u0987 \u09A7\u09BE\u09B0\u09A3\u09BE\u0997\u09C1\u09B2\u09BF \u09A6\
  \u09C7\u0996\u09BE\u09A8\u09CB \u098F\u0995\u099F\u09BF \u09AE\u09CC\u09B2\u09BF\
  \u0995 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE\
  \ \u09B9\u09B2\u09CB."
title: "\u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u09A4\u09C1\u09B2\
  \u09A8\u09BE \u0995\u09B0\u09BE"
weight: 27
---

## কিভাবে:
ডার্টে, আপনি `DateTime` ক্লাস ব্যবহার করে তারিখ তুলনা করতে পারেন, যা `isBefore`, `isAfter`, এবং `isAtSameMomentAs` এর মতো পদ্ধতিগুলি সরাসরি তুলনার জন্য সরবরাহ করে। অতিরিক্তভাবে, তারিখের মধ্যে পার্থক্য `difference()` পদ্ধতি ব্যবহার করে নির্ধারণ করা যায়, যা একটি `Duration` অবজেক্ট সরবরাহ করে যা দুটি সময় বিন্দুর মধ্যে ব্যবধান বিস্তারিত করে।

নিম্নে এই ধারণাগুলি দেখানো একটি মৌলিক উদাহরণ দেওয়া হলো:

```dart
void main() {
  DateTime eventStart = DateTime(2023, 5, 15);
  DateTime eventEnd = DateTime(2023, 5, 20);
  
  // যাচাই করা হচ্ছে যে একটি তারিখ আরেকটি তারিখের আগে কিনা
  if (eventStart.isBefore(eventEnd)) {
    print("ইভেন্টের শুরুর তারিখ ইভেন্টের শেষ তারিখের আগে রয়েছে।");
  }

  // যাচাই করা হচ্ছে যে দুটি তারিখ একই কিনা
  if (!eventStart.isAtSameMomentAs(eventEnd)) {
    print("শুরুর এবং শেষের তারিখগুলি একই নয়।");
  }
  
  // দুটি তারিখের মধ্যে পার্থক্য গণনা করা হচ্ছে
  Duration eventDuration = eventEnd.difference(eventStart);
  print("ইভেন্টটি ${eventDuration.inDays} দিন স্থায়ী হয়।");
}

/*
আউটপুট:
ইভেন্টের শুরুর তারিখ ইভেন্টের শেষ তারিখের আগে রয়েছে।
শুরুর এবং শেষের তারিখগুলি একই নয়।
ইভেন্টটি 5 দিন স্থায়ী হয়।
*/
```

তারিখের আরও উন্নত ম্যানিপুলেশনের জন্য, যেমন ফর্ম্যাট রূপান্তর, আপনি `intl` প্যাকেজ থেকে `DateFormat` ক্লাস উপকারী হিসেবে পাবেন। নিচে এর ব্যবহার করে তারিখ ফর্ম্যাটিং এবং তুলনা করার উপায় দেখানো হয়েছে:

প্রথমে, আপনার `pubspec.yaml` এ `intl` প্যাকেজ অন্তর্ভুক্ত করুন:

```yaml
dependencies:
  intl: ^0.17.0
```

তারপর, এটি নিম্নরূপ ব্যবহার করুন:

```dart
import 'package:intl/intl.dart';

void main() {
  DateTime departureDate = DateTime(2023, 5, 15);
  DateTime returnDate = DateTime.parse('2023-05-20');

  // তারিখগুলি ফর্ম্যাটিং করা হচ্ছে
  var formatter = DateFormat('yyyy-MM-dd');
  print("প্রস্থান: ${formatter.format(departureDate)}");
  print("প্রত্যাবর্তন: ${formatter.format(returnDate)}");

  // ফরম্যাটেড স্ট্রিংগুলি ব্যবহার করে তুলনা করা হচ্ছে
  if (formatter.format(departureDate) == formatter.format(returnDate)) {
    print("প্রস্থান এবং প্রত্যাবর্তনের তারিখগুলি একই।");
  } else {
    print("প্রস্থান এবং প্রত্যাবর্তনের তারিখগুলি ভিন্ন।");
  }
}

/*
আউটপুট:
প্রস্থান: 2023-05-15
প্রত্যাবর্তন: 2023-05-20
প্রস্থান এবং প্রত্যাবর্তনের তারিখগুলি ভিন্ন।
*/
```

এই উদাহরণটি দেখায় কিভাবে দুটি `DateTime` অবজেক্টের সরাসরি এবং নির্দিষ্ট উপাদানগুলি যেমন সময় উপেক্ষা করে তুলনা প্রয়োজন হলে ফর্ম্যাটিং করা স্ট্রিংগুলি ব্যবহার করা যায়।

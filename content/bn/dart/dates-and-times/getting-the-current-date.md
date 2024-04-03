---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:32.495721-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Dart \u098F\u09B0 \u09AE\u09C2\
  \u09B2 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF `DateTime` \u0995\u09CD\
  \u09B2\u09BE\u09B8\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09AC\
  \u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996 \u0993 \u09B8\
  \u09AE\u09AF\u09BC \u09AA\u09C7\u09A4\u09C7 \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF\
  \ \u09AA\u09CD\u09B0\u09AC\u09C7\u09B6 \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\
  \u09B0\u09C7\u0964 \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\
  \u09BF\u0996 \u09AA\u09BE\u0993\u09AF\u09BC\u09BE\u09B0 \u09AE\u09CC\u09B2\u09BF\
  \u0995 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09B9\u09B2."
lastmod: '2024-03-17T18:47:43.730307-06:00'
model: gpt-4-0125-preview
summary: "Dart \u098F\u09B0 \u09AE\u09C2\u09B2 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\
  \u09C7\u09B0\u09BF `DateTime` \u0995\u09CD\u09B2\u09BE\u09B8\u09C7\u09B0 \u09AE\u09BE\
  \u09A7\u09CD\u09AF\u09AE\u09C7 \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\
  \u09BE\u09B0\u09BF\u0996 \u0993 \u09B8\u09AE\u09AF\u09BC \u09AA\u09C7\u09A4\u09C7\
  \ \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF \u09AA\u09CD\u09B0\u09AC\u09C7\u09B6 \u09AA\
  \u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964 \u09AC\u09B0\u09CD\u09A4\
  \u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u0993\u09AF\u09BC\
  \u09BE\u09B0 \u09AE\u09CC\u09B2\u09BF\u0995 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3\
  \ \u09B9\u09B2."
title: "\u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u09AA\u09C7\u09A4\u09C7"
weight: 29
---

## কিভাবে:
Dart এর মূল লাইব্রেরি `DateTime` ক্লাসের মাধ্যমে বর্তমান তারিখ ও সময় পেতে সরাসরি প্রবেশ প্রদান করে। বর্তমান তারিখ পাওয়ার মৌলিক উদাহরণ হল:

```dart
void main() {
  DateTime now = DateTime.now();
  print(now); // উদাহরন আউটপুট: 2023-04-12 10:00:00.000
}
```

যদি আপনি শুধুমাত্র তারিখ অংশটুকু (বছর, মাস, দিন) প্রয়োজন হয়, তাহলে `DateTime` অবজেক্টটি ফর্ম্যাট করতে পারেন:

```dart
void main() {
  DateTime now = DateTime.now();
  String formattedDate = "${now.year}-${now.month}-${now.day}";
  print(formattedDate); // উদাহরন আউটপুট: 2023-04-12
}
```

Dart জটিল তারিখ ফর্ম্যাটিংয়ের জন্য কোনো অন্তর্ভুক্ত লাইব্রেরি রাখে না, তবে এই উদ্দেশ্যে আপনি `intl` প্যাকেজ ব্যবহার করতে পারেন। প্রথমে, আপনার `pubspec.yaml` এ প্যাকেজটি যোগ করুন:

```yaml
dependencies:
  intl: ^0.17.0
```

তারপর, আপনি সহজেই তারিখগুলি ফর্ম্যাট করতে পারেন:

```dart
import 'package:intl/intl.dart';

void main() {
  DateTime now = DateTime.now();
  String formattedDate = DateFormat('yyyy-MM-dd').format(now);
  print(formattedDate); // উদাহরন আউটপুট: 2023-04-12
}
```

আরো উন্নত ফর্ম্যাটিং অপশন অন্বেষণ করতে, `intl` প্যাকেজ দ্বারা প্রদত্ত `DateFormat` ক্লাসটি পরীক্ষা করুন, যা বিভিন্ন ধরণের প্যাটার্ন এবং লোকেলগুলি সমর্থন করে।

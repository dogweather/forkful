---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:42:48.566614-06:00
description: "Dart \u098F \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\
  \u09B0\u09CD\u09A1 \u098F\u09B0\u09B0 (stderr) \u098F \u09B2\u09C7\u0996\u09BE \u09AE\
  \u09BE\u09A8\u09C7 \u09B9\u09B2 \u098F\u09B0\u09B0 \u09AE\u09CD\u09AF\u09BE\u09B8\
  \u09C7\u099C \u098F\u09AC\u0982 \u09A1\u09BE\u09DF\u09BE\u0997\u09A8\u09B8\u09CD\
  \u099F\u09BF\u0995\u09B8 \u098F\u0995\u099F\u09BF \u09AA\u09C3\u09A5\u0995 \u09B8\
  \u09CD\u099F\u09CD\u09B0\u09BF\u09AE\u09C7 \u09AA\u09BE\u09A0\u09BE\u09A8\u09CB\
  , \u09AF\u09BE \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\
  \u09CD\u09A1 \u0986\u0989\u099F\u09AA\u09C1\u099F (stdout) \u09A5\u09C7\u0995\u09C7\
  \ \u0986\u09B2\u09BE\u09A6\u09BE\u0964\u2026"
lastmod: '2024-03-17T18:47:43.737335-06:00'
model: gpt-4-0125-preview
summary: "Dart \u098F \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\
  \u09B0\u09CD\u09A1 \u098F\u09B0\u09B0 (stderr) \u098F \u09B2\u09C7\u0996\u09BE \u09AE\
  \u09BE\u09A8\u09C7 \u09B9\u09B2 \u098F\u09B0\u09B0 \u09AE\u09CD\u09AF\u09BE\u09B8\
  \u09C7\u099C \u098F\u09AC\u0982 \u09A1\u09BE\u09DF\u09BE\u0997\u09A8\u09B8\u09CD\
  \u099F\u09BF\u0995\u09B8 \u098F\u0995\u099F\u09BF \u09AA\u09C3\u09A5\u0995 \u09B8\
  \u09CD\u099F\u09CD\u09B0\u09BF\u09AE\u09C7 \u09AA\u09BE\u09A0\u09BE\u09A8\u09CB\
  , \u09AF\u09BE \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\
  \u09CD\u09A1 \u0986\u0989\u099F\u09AA\u09C1\u099F (stdout) \u09A5\u09C7\u0995\u09C7\
  \ \u0986\u09B2\u09BE\u09A6\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09C7\u09A8\
  \ \u09A8\u09B0\u09AE\u09BE\u09B2 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\
  \u09AE \u0986\u0989\u099F\u09AA\u09C1\u099F \u098F\u09AC\u0982 \u098F\u09B0\u09B0\
  \ \u09AC\u09BE \u0993\u09AF\u09BC\u09BE\u09B0\u09CD\u09A8\u09BF\u0982 \u09AE\u09CD\
  \u09AF\u09BE\u09B8\u09C7\u099C\u0997\u09C1\u09B2\u09BF\u09B0 \u09AE\u09A7\u09CD\u09AF\
  \u09C7 \u09AA\u09BE\u09B0\u09CD\u09A5\u0995\u09CD\u09AF \u0995\u09B0\u09A4\u09C7\
  , \u09AF\u09BE \u09A1\u09BF\u09AC\u09BE\u0997\u09BF\u0982 \u098F\u09AC\u0982 \u09B2\
  \u0997\u09BF\u0982\u0995\u09C7 \u0986\u09B0\u0993 \u09B8\u09B9\u099C \u0995\u09B0\
  \u09C7 \u09A4\u09CB\u09B2\u09C7\u0964."
title: "\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u098F\u09B0\u09B0\u09C7 \u09B2\u09BF\u0996\u09A8"
weight: 25
---

## কিভাবে:
Dart এ, `dart:io` তে উপলব্ধ `stderr` অবজেক্ট ব্যবহার করে stderr এ লেখা অত্যন্ত সহজ। এখানে একটি বেসিক উদাহরণ দেওয়া হল:

```dart
import 'dart:io';

void main() {
  stderr.writeln('এটি একটি এরর ম্যাসেজ।');
}
```

চালানো হলে আউটপুট:
```
এটি একটি এরর ম্যাসেজ।
```
এই ম্যাসেজটি stderr স্ট্রিমে পাঠানো হয়, যা সাধারণত কনসোল বা টার্মিনালে দেখানো হয়।

একটি বেশি জটিলতার উদাহরণ দেখানোর জন্য, যেমন একটি ব্যতিক্রম লগিং করা, Dart এর সমৃদ্ধ সেট অফ ফিচারগুলি সংক্ষিপ্ত এবং কার্যকরী এরর হ্যান্ডলিং অনুমোদন করে:

```dart
import 'dart:io';

void riskyOperation() {
  try {
    // এমন একটি অপারেশন অনুমান করুন যা থ্রো করতে পারে
    throw Exception('কিছু ভুল হয়ে গেছে!');
  } catch (e) {
    stderr.writeln('এরর: $e');
  }
}

void main() {
  riskyOperation();
}
```

চালানো হলে আউটপুট:
```
এরর: Exception: কিছু ভুল হয়ে গেছে!
```

এই প্যাটার্নটি বিশেষ করে তাদের জন্য দরকারী যাদের সাধারণ লগ থেকে এরর লগগুলি পৃথক করা দরকার, যা অ্যাপ্লিকেশনগুলোকে মনিটর এবং ডিবাগ করা আরও সহজ করে তোলে।

Dart এর স্ট্যান্ডার্ড লাইব্রেরি বেশ সম্পূর্ণ, তবে অনেক প্রোগ্রামের stderr এ লেখার জন্য থার্ড-পার্টি লাইব্রেরিগুলির প্রয়োজন হয় না। তবে, যদি আপনার অ্যাপ্লিকেশনের জন্য আরও উন্নত লগিং ক্ষমতা প্রয়োজন হয় (যেমন, ফাইলে, নেটওয়ার্কের মাধ্যমে, ফর্ম্যাটিং), `logging` প্যাকেজটি একটি জনপ্রিয় পছন্দ। এখানে `logging` দ্বারা এরর ব্যবহারের একটি দ্রুত দৃষ্টান্ত দেওয়া হল:

```dart
import 'dart:io';
import 'package:logging/logging.dart';

final logger = Logger('MyAppLogger');

void setupLogging() {
  logger.onRecord.listen((record) {
    if (record.level >= Level.SEVERE) {
      stderr.writeln('${record.level.name}: ${record.time}: ${record.message}');
    }
  });
}

void main() {
  setupLogging();
  logger.severe('Severe Error: কিছু গুরুতর খারাপ ঘটেছে।');
}
```

চালানো হলে আউটপুট:
```
SEVERE: 2023-04-01 00:00:00.000: Severe Error: কিছু গুরুতর খারাপ ঘটেছে।
```

এই পদ্ধতিটি কি হিসেবে এরর লগ করা হবে এবং কিভাবে তা ফর্ম্যাট করা হবে সে ব্যাপারে আরও উচ্চতর ডিগ্রির কাস্টমাইজেশন এবং নিয়ন্ত্রণ অফার করে, যা বৃহত্তর, আরও জটিল অ্যাপ্লিকেশনগুলির জন্য খুবই সহায়ক।

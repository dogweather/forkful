---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:49.526209-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09A1\u09BE\u09B0\u09CD\u099F\
  \ \u09A4\u09BE\u09B0 `DateTime` \u0995\u09CD\u09B2\u09BE\u09B8\u09C7\u09B0 \u09AE\
  \u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\u09B0\
  \ \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\u09AA\u09C1\u09B2\u09C7\u09B6\u09A8\u09C7\
  \u09B0 \u099C\u09A8\u09CD\u09AF \u09A6\u09C3\u09A2\u09BC \u09B8\u09AE\u09B0\u09CD\
  \u09A5\u09A8 \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964 \u098F\
  \u0996\u09BE\u09A8\u09C7 \u09A6\u09C7\u0996\u09BE\u09A8\u09CB \u09B9\u09AC\u09C7\
  \ \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u09A8\u09C7\u099F\u09BF\u09AD \u09A1\u09BE\
  \u09B0\u09CD\u099F \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \ \u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09CE\u2026"
lastmod: '2024-03-17T18:47:43.733834-06:00'
model: gpt-4-0125-preview
summary: "\u09A1\u09BE\u09B0\u09CD\u099F \u09A4\u09BE\u09B0 `DateTime` \u0995\u09CD\
  \u09B2\u09BE\u09B8\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09A4\
  \u09BE\u09B0\u09BF\u0996\u09C7\u09B0 \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\u09AA\u09C1\
  \u09B2\u09C7\u09B6\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09A6\u09C3\u09A2\
  \u09BC \u09B8\u09AE\u09B0\u09CD\u09A5\u09A8 \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8\
  \ \u0995\u09B0\u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u09A6\u09C7\u0996\u09BE\
  \u09A8\u09CB \u09B9\u09AC\u09C7 \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u09A8\u09C7\
  \u099F\u09BF\u09AD \u09A1\u09BE\u09B0\u09CD\u099F \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09C7 \u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09CE \u0985\
  \u09A5\u09AC\u09BE \u0985\u09A4\u09C0\u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\
  \u0996 \u0997\u09A3\u09A8\u09BE \u0995\u09B0\u09BE \u09AF\u09BE\u09AF\u09BC, \u09A4\
  \u09C3\u09A4\u09C0\u09AF\u09BC-\u09AA\u0995\u09CD\u09B7\u09C7\u09B0 \u09B2\u09BE\
  \u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u0997\u09C1\u09B2\u09BF\u09B0 \u09AA\u09CD\
  \u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u099B\u09BE\u09A1\u09BC\u09BE\u0964\n\n#."
title: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u09AC\u09BE \u0985\u09A4\u09C0\
  \u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u0997\u09A3\u09A8\u09BE \u0995\
  \u09B0\u09BE"
weight: 26
---

## কিভাবে:
ডার্ট তার `DateTime` ক্লাসের মাধ্যমে তারিখের ম্যানিপুলেশনের জন্য দৃঢ় সমর্থন প্রদান করে। এখানে দেখানো হবে কিভাবে নেটিভ ডার্ট ব্যবহার করে ভবিষ্যৎ অথবা অতীতের তারিখ গণনা করা যায়, তৃতীয়-পক্ষের লাইব্রেরিগুলির প্রয়োজন ছাড়া।

### ভবিষ্যতের তারিখ গণনা
ভবিষ্যতে একটি তারিখ গণনা করার জন্য, আপনি একটি `DateTime` অবজেক্ট তৈরি করেন এবং প্রার্থিত সময়কাল সহ `add` মেথড ব্যবহার করেন।

```dart
DateTime today = DateTime.now();
Duration tenDays = Duration(days: 10);
DateTime futureDate = today.add(tenDays);

print(futureDate); // আউটপুট: 2023-04-21 14:22:35.123456 (উদাহরণ আউটপুট, বর্তমান তারিখ ও সময়ের উপর নির্ভর করে)
```

### অতীতের তারিখ গণনা
অতীতে একটি তারিখ গণনা করার জন্য, আপনি প্রয়োজনীয় সময়কাল সহ `DateTime` অবজেক্টে `subtract` মেথড ব্যবহার করেন।

```dart
DateTime today = DateTime.now();
Duration fifteenDaysAgo = Duration(days: 15);
DateTime pastDate = today.subtract(fifteenDaysAgo);

print(pastDate); // আউটপুট: 2023-03-27 14:22:35.123456 (উদাহরণ আউটপুট, বর্তমান তারিখ ও সময়ের উপর নির্ভর করে)
```

### তৃতীয়-পক্ষের লাইব্রেরিগুলি ব্যবহার করা
যদিও ডার্টের নেটিভ সামর্থ্য তারিখের ম্যানিপুলেশনের জন্য শক্তিশালী, আপনি নিজেকে আরো বিশেষ পরিচালনা প্রয়োগের প্রয়োজন বোধ করতে পারেন, যেমন তারিখ বিশ্লেষণ অথবা ফর্ম্যাটিং আরো সহজে করা, অথবা জটিল গণনা সম্পাদন করা। এইরকম ক্ষেত্রে, `time` প্যাকেজ অত্যন্ত উপকারী হতে পারে।

প্রথমে, আপনার `pubspec.yaml` নির্ভরতাতে `time` যোগ করুন:

```yaml
dependencies:
  time: ^2.0.0
```

এরপর, আপনি এটি ব্যবহার করে উন্নত পাঠ্যতা সহ অনুরূপ গণনা সম্পাদন করতে পারেন:

```dart
import 'package:time/time.dart';

void main() {
  DateTime today = DateTime.now();

  // ভবিষ্যতের তারিখ গণনা
  DateTime futureDate = today + 10.days;
  print(futureDate); // আউটপুট ফরম্যাট: 2023-04-21 14:22:35.123456

  // অতীতের তারিখ গণনা
  DateTime pastDate = today - 15.days;
  print(pastDate); // আউটপুট ফরম্যাট: 2023-03-27 14:22:35.123456
}
```

এই উদাহরণগুলি ডার্টে তারিখের মূল ম্যানিপুলেশনগুলি দেখায়, বর্তমান তারিখ থেকে সময় যোগ করা বা বিয়োগ করা সহ, দেখিয়ে দেয় যে ডার্ট অ্যাপ্লিকেশনগুলিতে তারিখগুলি কত সহজে পরিচালনা করা যেতে পারে।

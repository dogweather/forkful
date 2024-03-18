---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:23:06.388512-06:00
description: "Dart-\u098F\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF\
  \ \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u200C\u09CD\u09AF\u09BE\u0995\u09CD\u099F\
  \u09BF\u09AD \u09B6\u09C7\u09B2 (REPL - Read-Evaluate-Print Loop) \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09A6\u09C7\u09B0\u0995\u09C7 \u09B8\
  \u09AE\u09CD\u09AA\u09C2\u09B0\u09CD\u09A3 \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\
  \u09CD\u099F \u0995\u09AE\u09CD\u09AA\u09BE\u0987\u09B2 \u0995\u09B0\u09BE \u099B\
  \u09BE\u09DC\u09BE \u09A1\u09BE\u09B0\u09CD\u099F \u0995\u09CB\u09A1 \u09B2\u09BE\
  \u0987\u09A8 \u09AC\u09BE\u0987 \u09B2\u09BE\u0987\u09A8\u2026"
lastmod: '2024-03-17T18:47:43.720546-06:00'
model: gpt-4-0125-preview
summary: "Dart-\u098F\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF \u0987\
  \u09A8\u09CD\u099F\u09BE\u09B0\u200C\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09BF\u09AD\
  \ \u09B6\u09C7\u09B2 (REPL - Read-Evaluate-Print Loop) \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09A6\u09C7\u09B0\u0995\u09C7 \u09B8\u09AE\u09CD\
  \u09AA\u09C2\u09B0\u09CD\u09A3 \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\
  \ \u0995\u09AE\u09CD\u09AA\u09BE\u0987\u09B2 \u0995\u09B0\u09BE \u099B\u09BE\u09DC\
  \u09BE \u09A1\u09BE\u09B0\u09CD\u099F \u0995\u09CB\u09A1 \u09B2\u09BE\u0987\u09A8\
  \ \u09AC\u09BE\u0987 \u09B2\u09BE\u0987\u09A8\u2026"
title: "\u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AF\u09BC\u09BE\u0995\u09CD\u099F\u09BF\
  \u09AD \u09B6\u09C7\u09B2 (REPL) \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09BE"
---

{{< edit_this_page >}}

## কি ও কেন?

Dart-এর জন্য একটি ইন্টার‌্যাক্টিভ শেল (REPL - Read-Evaluate-Print Loop) প্রোগ্রামারদেরকে সম্পূর্ণ স্ক্রিপ্ট কম্পাইল করা ছাড়া ডার্ট কোড লাইন বাই লাইন ডায়নামিকভাবে টাইপ ও রান করার অনুমতি দেয়। এই টুলটি ডার্টের সিনট্যাক্স শেখা, কোড স্নিপেট নিয়ে পরীক্ষা করা অথবা ইন্সট্যান্ট ফিডব্যাক ও ক্রমাগত টেস্টিং সুবিধা দিয়ে ডিবাগিং করার ক্ষেত্রে অমূল্য।

## কিভাবে:

Dart এর মধ্যে কোনো বিল্ট-ইন REPL নেই। তবে, DartPad (অনলাইন) ব্যবহার করে অথবা `dart_repl` এর মত থার্ড-পার্টি টুলস ব্যবহার করে REPL-এর মত কার্যকারিতা অর্জন করা সম্ভব।

**DartPad ব্যবহার করে:**

DartPad (https://dartpad.dev) হলো একটি অনলাইন ডার্ট এডিটর যেটি আপনাকে আপনার ওয়েব ব্রাউজারে ডার্ট কোড লেখা ও রান করতে দেয়। যদিও এটি একটি প্রথাগত কমান্ড-লাইন REPL নয়, তবে এটি দ্রুত পরীক্ষণের জন্য অনুরূপ অভিজ্ঞতা প্রদান করে।

ওয়েবসাইটে যান, বাম প্যানেলে আপনার ডার্ট কোড টাইপ করুন এবং রেজাল্ট ডান পাশে দেখতে "Run" বাটনে ক্লিক করুন।

উদাহরণ:
```dart
void main() {
  print('Hello, Dart!');
}
```
আউটপুট:
```
Hello, Dart!
```

**`dart_repl` ব্যবহার করে (থার্ড-পার্টি টুল):**

প্রথমে, `dart_repl` কে পাবের মাধ্যমে গ্লোবালি ইনস্টল করুন:

```shell
dart pub global activate dart_repl
```

তারপর, আপনার টার্মিনাল থেকে `dart_repl` রান করুন:

```shell
dart_repl
```

এখন, আপনি সরাসরি শেলে ডার্ট স্টেটমেন্ট টাইপ করা শুরু করতে পারেন। উদাহরণস্বরূপ:

```dart
>>> print('Hello, REPL!');
Hello, REPL!
>>> int add(int x, int y) => x + y;
>>> print(add(5, 7));
12
```

এই পদ্ধতিগুলি মুহূর্তে ডার্ট কোড চেষ্টা করার জন্য একটি ত্বরান্বিত পথ প্রদান করে, শিখন প্রক্রিয়াকে সহজ করে এবং উৎপাদনশীলতা বৃদ্ধি করে।

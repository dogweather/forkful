---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:14:16.212460-06:00
description: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE\u0995\u09C7 \u0997\u09CB\u09B2\
  \ \u0995\u09B0\u09BE \u09B9\u09B2 \u09B8\u09C7\u0987 \u09B8\u0982\u0996\u09CD\u09AF\
  \u09BE\u099F\u09BF\u0995\u09C7 \u09A4\u09BE\u09B0 \u09A8\u09BF\u0995\u099F\u09A4\
  \u09AE \u09AA\u09C2\u09B0\u09CD\u09A3 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u0985\
  \u09A5\u09AC\u09BE \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09A6\
  \u09B6\u09AE\u09BF\u0995 \u09B8\u09CD\u09A5\u09BE\u09A8 \u09AA\u09B0\u09CD\u09AF\
  \u09A8\u09CD\u09A4 \u09B8\u09BE\u09AE\u099E\u09CD\u099C\u09B8\u09CD\u09AF \u0995\
  \u09B0\u09BE\u09B0 \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u09AA\u09CD\u09B0\u09BE\u09AF\u09BC\u0987 \u0997\u09A3\u09A8\u09BE\u0995\u09C7\
  \ \u09B8\u09B0\u09B2\u2026"
lastmod: '2024-03-17T18:47:43.712613-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE\u0995\u09C7 \u0997\u09CB\u09B2 \u0995\
  \u09B0\u09BE \u09B9\u09B2 \u09B8\u09C7\u0987 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\
  \u099F\u09BF\u0995\u09C7 \u09A4\u09BE\u09B0 \u09A8\u09BF\u0995\u099F\u09A4\u09AE\
  \ \u09AA\u09C2\u09B0\u09CD\u09A3 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u0985\u09A5\
  \u09AC\u09BE \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09A6\u09B6\
  \u09AE\u09BF\u0995 \u09B8\u09CD\u09A5\u09BE\u09A8 \u09AA\u09B0\u09CD\u09AF\u09A8\
  \u09CD\u09A4 \u09B8\u09BE\u09AE\u099E\u09CD\u099C\u09B8\u09CD\u09AF \u0995\u09B0\
  \u09BE\u09B0 \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u09AA\u09CD\u09B0\u09BE\u09AF\u09BC\u0987 \u0997\u09A3\u09A8\u09BE\u0995\u09C7\
  \ \u09B8\u09B0\u09B2\u2026"
title: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09A8\u09BF\u09B0\u09CD\u09A3\u09DF"
---

{{< edit_this_page >}}

## কি এবং কেন?

সংখ্যাকে গোল করা হল সেই সংখ্যাটিকে তার নিকটতম পূর্ণ সংখ্যা অথবা নির্দিষ্ট দশমিক স্থান পর্যন্ত সামঞ্জস্য করার প্রক্রিয়া। প্রোগ্রামাররা প্রায়ই গণনাকে সরল করতে, পঠনযোগ্যতা বাড়াতে অথবা ডেটা প্রদর্শনের জন্য প্রস্তুত করতে সংখ্যাগুলি গোল করে। এটি সংখ্যালঘু আউটপুটে স্থিতিশীলতা এবং স্পষ্টতা নিশ্চিত করে।

## কিভাবে:

Dart তার core `num` টাইপে গোল করার অপারেশনের জন্য নেটিভ মেথড সরবরাহ করে। এখানে, আমরা `round()`, `floor()`, `ceil()` এবং নির্দিষ্ট সংখ্যক দশমিক স্থানে গোল করার মতো মেথড সম্পর্কে জানব।

### নিকটতম পূর্ণ সংখ্যায় গোল করা:

```dart
var number = 3.56;
print(number.round()); // আউটপুট: 4
```

### নিচে গোল করা:

```dart
print(number.floor()); // আউটপুট: 3
```

### উপরে গোল করা:

```dart
print(number.ceil()); // আউটপুট: 4
```

### নির্দিষ্ট সংখ্যক দশমিক স্থানে গোল করা:

নির্দিষ্ট সংখ্যক দশমিক স্থানে গোল করার জন্য, আমরা `toStringAsFixed()` মেথড ব্যবহার করতে পারি, যা একটি স্ট্রিং ফেরত দেয়, অথবা সাংখ্যিক ফলাফলের জন্য `dart:math`-এর `pow` এর সংমিশ্রণ ব্যবহার করতে পারি।

```dart
import 'dart:math';

var number = 3.56789;
String roundedString = number.toStringAsFixed(2); // প্রদর্শনের উদ্দেশ্যে
print(roundedString); // আউটপুট: 3.57

double roundedNumber = double.parse(roundedString);
print(roundedNumber); // আউটপুট: 3.57

// বিকল্পভাবে, একটি সাংখ্যিক ফলাফলের জন্য:
double roundedToDecimal = (number * pow(10, 2)).round().toDouble() / pow(10, 2);
print(roundedToDecimal); // আউটপুট: 3.57
```

যদিও Dart-এর কোর লাইব্রেরি বেশিরভাগ গোল করার প্রয়োজনীয়তা কার্যকরভাবে মেটায়, আরো জটিল গাণিতিক অপারেশন অথবা নির্দিষ্ট গোল করার প্রয়োজনের জন্য, `decimal` মত লাইব্রেরি সুবিধাজনক হতে পারে। `decimal` লাইব্রেরি নির্ভুলতা হারানো ছাড়াই দশমিক সংখ্যা নিয়ে সহজে কাজ করার একটি সুবিধাজনক উপায় সরবরাহ করে, যা বিশেষভাবে অর্থনৈতিক গণনা জন্য দরকারী, কিন্তু সহজ গোল করার পদ্ধতির জন্য যেমন দেখানো হয়েছে, Dart কোর ফাংশনালিটি সাধারণত যথেষ্ট হয়।

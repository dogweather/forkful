---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:24.176014-06:00
description: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\
  \u09C7 \u09AC\u09A1\u09BC\u09B9\u09BE\u09A4\u09C7\u09B0 \u0985\u0995\u09CD\u09B7\
  \u09B0\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE\
  \ \u09AE\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09B6\u09AC\u09CD\u09A6 \u0985\
  \u09A5\u09AC\u09BE \u09AA\u09C1\u09B0\u09CB \u09AC\u09BE\u0995\u09CD\u09AF\u09C7\
  \u09B0 \u09AA\u09CD\u09B0\u09A5\u09AE \u0985\u0995\u09CD\u09B7\u09B0\u099F\u09BF\
  \ \u0995\u09C7 \u09AC\u09A1\u09BC \u0985\u0995\u09CD\u09B7\u09B0\u09C7 \u09AA\u09B0\
  \u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\u09BE, \u098F\u09AC\u0982 \u09AC\
  \u09BE\u0995\u09BF \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09CB\u0995\u09C7\
  \ \u09AF\u09C7\u09AE\u09A8 \u0986\u099B\u09C7 \u09A4\u09C7\u09AE\u09A8\u09BF\u2026"
lastmod: '2024-03-17T18:47:43.698594-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\
  \u09C7 \u09AC\u09A1\u09BC\u09B9\u09BE\u09A4\u09C7\u09B0 \u0985\u0995\u09CD\u09B7\
  \u09B0\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE\
  \ \u09AE\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09B6\u09AC\u09CD\u09A6 \u0985\
  \u09A5\u09AC\u09BE \u09AA\u09C1\u09B0\u09CB \u09AC\u09BE\u0995\u09CD\u09AF\u09C7\
  \u09B0 \u09AA\u09CD\u09B0\u09A5\u09AE \u0985\u0995\u09CD\u09B7\u09B0\u099F\u09BF\
  \ \u0995\u09C7 \u09AC\u09A1\u09BC \u0985\u0995\u09CD\u09B7\u09B0\u09C7 \u09AA\u09B0\
  \u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\u09BE, \u098F\u09AC\u0982 \u09AC\
  \u09BE\u0995\u09BF \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09CB\u0995\u09C7\
  \ \u09AF\u09C7\u09AE\u09A8 \u0986\u099B\u09C7 \u09A4\u09C7\u09AE\u09A8\u09BF\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09AA\u09CD\u09B0\
  \u09A5\u09AE \u0985\u0995\u09CD\u09B7\u09B0 \u09AC\u09A1\u09BC \u09B9\u09BE\u09A4\
  \u09C7\u09B0 \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি ও কেন?

একটি স্ট্রিংকে বড়হাতের অক্ষরে রূপান্তর করা মানে একটি শব্দ অথবা পুরো বাক্যের প্রথম অক্ষরটি কে বড় অক্ষরে পরিবর্তন করা, এবং বাকি অক্ষরগুলোকে যেমন আছে তেমনি রাখা। প্রোগ্রামাররা প্রায়ই ব্যবহারকারীদের ইনপুট ফর্ম্যাটিং করা বা টেক্সট প্রদর্শনের সময় ধারাবাহিকতা নিশ্চিত করা অথবা ব্যবহারকারীদের ইন্টারফেইসে ব্যাকরণের নিয়ম মেনে চলার জন্য এই পদ্ধতিটি ব্যবহার করে।

## কিভাবে:

### ডার্টের বিল্ট-ইন পদ্ধতি ব্যবহার করে

ডার্ট স্ট্রিং ম্যানিপুলেশনের সহজ ও সরাসরি পদ্ধতি প্রদান করে। একটি শব্দ অথবা বাক্যকে বড়হাতের অক্ষরে রূপান্তর করার জন্য, আপনি সাধারণত প্রথম অক্ষরটি নিয়ে, তাকে বড় অক্ষরে পরিবর্তন করে, তারপর বাকি স্ট্রিংটির সাথে জোড়া দিবেন। এটি কিভাবে বাস্তবায়ন করা যায় তা হলো:

```dart
String capitalize(String text) {
  if (text.isEmpty) return text;
  return text[0].toUpperCase() + text.substring(1).toLowerCase();
}

void main() {
  var example = "hello world";
  print(capitalize(example)); // আউটপুট: Hello world
}
```

### প্রতিটি শব্দের প্রথম অক্ষর বড়হাতে করা

একটি স্ট্রিং এর প্রতিটি শব্দের প্রথম অক্ষর বড়হাতে করার জন্য, আপনি স্ট্রিংটিকে শব্দে শব্দে ভাগ করতে পারেন, প্রতিটি শব্দের প্রথম অক্ষরটি বড় অক্ষরে রূপান্তরিত করে, তারপর সেগুলোকে আবার একত্রিত করতে পারেন:

```dart
String capitalizeWords(String text) {
  return text.split(' ').map(capitalize).join(' ');
}

void main() {
  var example = "hello dart enthusiasts";
  print(capitalizeWords(example)); // আউটপুট: Hello Dart Enthusiasts
}
```

### থার্ড-পার্টি লাইব্রেরি ব্যবহার করে

যদিও ডার্টের মানক লাইব্রেরি মৌলিক প্রয়োজনীয়তা পূরণ করে, কিছু কাজ থার্ড-পার্টি প্যাকেজ ব্যবহার করে আরও সুবিধাজনক ভাবে সম্পন্ন করা যেতে পারে। স্ট্রিং ম্যানিপুলেশনের ব্যাপক আওতা, যেমন বড়হাতের অক্ষরে রূপান্তরসহ অন্যান্য ফাংশনালিটি, সম্পাদনের জন্য একটি জনপ্রিয় লাইব্রেরি হল [`recase`](https://pub.dev/packages/recase) প্যাকেজ। এটি আপনার প্রজেক্টের `pubspec.yaml`-এ যোগ করার পর, আপনি সহজেই স্ট্রিংগুলিকে বড়হাতের অক্ষরে রূপান্তর সহ অন্যান্য কাজ করতে পারবেন:

```dart
import 'package:recase/recase.dart';

void main() {
  var example = "hello world";
  var rc = ReCase(example);

  print(rc.titleCase); // আউটপুট: Hello World
}
```

`recase` ব্যবহার করে, আপনি পৃথক শব্দগুলি, পুরো বাক্যগুলি এমনকি অন্যান্য কেসিং কনভেনশনগুলিকেও ম্যানুয়ালি স্ট্রিং ট্রান্সফরমেশন সামলানো ছাড়াই বড়হাতের অক্ষরে রূপান্তর করতে পারেন।

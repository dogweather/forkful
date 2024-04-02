---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:39:43.196983-06:00
description: "\u09A1\u09BE\u09B0\u09CD\u099F\u09C7 \u098F\u0995\u099F\u09BF \u09AA\
  \u09BE\u09A0\u09CD\u09AF \u09AB\u09BE\u0987\u09B2 \u09B2\u09BF\u0996\u09BE \u0985\
  \u09B0\u09CD\u09A5 \u09A1\u09BF\u09B8\u09CD\u0995\u09C7 \u09AB\u09BE\u0987\u09B2\
  \u0997\u09C1\u09B2\u09BF \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE \u09AC\u09BE\
  \ \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\u09BE \u09AF\u09BE\
  \u09A4\u09C7 \u09A1\u09C7\u099F\u09BE \u09AA\u09BE\u09A0\u09CD\u09AF \u09AB\u09B0\
  \u09CD\u09AE\u09CD\u09AF\u09BE\u099F\u09C7 \u09B8\u0982\u09B0\u0995\u09CD\u09B7\u09A3\
  \ \u0995\u09B0\u09BE \u09AF\u09BE\u09DF\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09C7\
  \ \u09A5\u09BE\u0995\u09C7 \u098F\u09AA\u09CD\u09B2\u09BF\u0995\u09C7\u09B6\u09A8\
  \u2026"
lastmod: '2024-03-17T18:47:43.739375-06:00'
model: gpt-4-0125-preview
summary: "\u09A1\u09BE\u09B0\u09CD\u099F\u09C7 \u098F\u0995\u099F\u09BF \u09AA\u09BE\
  \u09A0\u09CD\u09AF \u09AB\u09BE\u0987\u09B2 \u09B2\u09BF\u0996\u09BE \u0985\u09B0\
  \u09CD\u09A5 \u09A1\u09BF\u09B8\u09CD\u0995\u09C7 \u09AB\u09BE\u0987\u09B2\u0997\
  \u09C1\u09B2\u09BF \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE \u09AC\u09BE \u09AA\
  \u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\u09BE \u09AF\u09BE\u09A4\
  \u09C7 \u09A1\u09C7\u099F\u09BE \u09AA\u09BE\u09A0\u09CD\u09AF \u09AB\u09B0\u09CD\
  \u09AE\u09CD\u09AF\u09BE\u099F\u09C7 \u09B8\u0982\u09B0\u0995\u09CD\u09B7\u09A3\
  \ \u0995\u09B0\u09BE \u09AF\u09BE\u09DF\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09C7\
  \ \u09A5\u09BE\u0995\u09C7 \u098F\u09AA\u09CD\u09B2\u09BF\u0995\u09C7\u09B6\u09A8\
  \u2026"
title: "\u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\
  \u0987\u09B2 \u09B2\u09BF\u0996\u09BE"
weight: 24
---

## কি ও কেন?
ডার্টে একটি পাঠ্য ফাইল লিখা অর্থ ডিস্কে ফাইলগুলি তৈরি করা বা পরিবর্তন করা যাতে ডেটা পাঠ্য ফর্ম্যাটে সংরক্ষণ করা যায়। প্রোগ্রামাররা এটি করে থাকে এপ্লিকেশন ডেটা, কনফিগারেশন, লগ বা যে কোনো তথ্য যা এপ্লিকেশন রানের মধ্যে বা অন্য অ্যাপ্লিকেশন বা ব্যবহারকারীদের মধ্যে ডেটা শেয়ার করার জন্য স্থায়ী করতে হয়।

## কিভাবে:
ডার্টের কোর লাইব্রেরি `dart:io` প্যাকেজ প্রদান করে যা ফাইল হ্যান্ডলিংয়ের জন্য এবং আপনাকে থার্ড-পার্টি লাইব্রেরির প্রয়োজন ছাড়াই পাঠ্য ফাইল লেখার অনুমতি দেয়। এখানে পাঠ্য ফাইল লেখার একটি সরল উদাহরণ দেওয়া হল:

```dart
import 'dart:io';

void main() async {
  // বর্তমান ডিরেক্টরিতে 'example.txt' নামে একটি নতুন ফাইল তৈরি করুন।
  var file = File('example.txt');
  
  // ফাইলে একটি স্ট্রিং লিখুন।
  await file.writeAsString('Hello, Dart!');
  
  // কন্টেন্টস যাচাই করুন।
  print(await file.readAsString()); // আউটপুট: Hello, Dart!
}
```

যখন বড় ফাইল বা ডেটার স্রোত নিয়ে কাজ করা হয়, তখন আপনি 'openWrite' ব্যবহার করে সামগ্রী লেখা পছন্দ করতে পারেন যা একটি `IOSink` ফেরত দেয় এবং আপনাকে ডাটা খন্ডাকারে লেখার অনুমতি দেয়:

```dart
import 'dart:io';

void main() async {
  var file = File('large_file.txt');
  var sink = file.openWrite();

  // ফাইলে একাধিক লাইন লিখুন।
  sink
    ..writeln('Line 1: The quick brown fox jumps over the lazy dog.')
    ..writeln('Line 2: Dart is awesome!')
    ..close();

  // সমস্ত ডেটা ফাইলে লিখে ফেলার জন্য সিঙ্ক বন্ধ হওয়ার অপেক্ষা করুন।
  await sink.done;

  // ফাইলের কন্টেন্ট পড়ে যাচাই করে দেখুন
  print(await file.readAsString());
}
```

ফাইল যোগ করা বা বাইট লেখার মতো আরও অগ্রসর ফাইল অপারেশনগুলির জন্য, আপনি `dart:io` দ্বারা প্রদত্ত `File` ক্লাস পদ্ধতির সবিস্তার বিবেচনা করতে পারেন। পাশাপাশি, বড় পরিসরের বা আরও জটিল প্রকল্পে কাজ করার সময়, ফাইল পথ নিয়ে কাজ করার জন্য 'path' বা ওয়েব সার্ভার ফাংশনালিটিজের জন্য 'shelf' এর মতো প্যাকেজ বিবেচনা করা সুবিধাজনক হতে পারে, যদিও সরাসরি ফাইল লেখা সাধারণত ডার্টের বিল্ট-ইন লাইব্রেরিগুলির উপর নির্ভর করে।

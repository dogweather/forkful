---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:16:02.413273-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Dart \u09A4\u09BE\u09B0 `String`\
  \ \u0995\u09CD\u09B2\u09BE\u09B8\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\
  \u09C7 \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F\
  \ \u0996\u09CB\u0981\u099C\u09BE \u098F\u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\
  \u09B8\u09CD\u09A5\u09BE\u09AA\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09B6\
  \u0995\u09CD\u09A4\u09BF\u09B6\u09BE\u09B2\u09C0 \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF\
  \ \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7, \u09AC\u09BE\u0987\u09B0\
  \u09C7\u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u0997\u09C1\u09B2\
  \u09BF\u09B0 \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u099B\u09BE\u09A1\
  \u09BC\u09BE\u0987\u0964\u2026"
lastmod: '2024-03-17T18:47:43.700861-06:00'
model: gpt-4-0125-preview
summary: "Dart \u09A4\u09BE\u09B0 `String` \u0995\u09CD\u09B2\u09BE\u09B8\u09C7\u09B0\
  \ \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF\
  \ \u099F\u09C7\u0995\u09CD\u09B8\u099F \u0996\u09CB\u0981\u099C\u09BE \u098F\u09AC\
  \u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\u09A5\u09BE\u09AA\u09A8\u09C7\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u09B6\u0995\u09CD\u09A4\u09BF\u09B6\u09BE\u09B2\u09C0\
  \ \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\
  \u09B0\u09C7, \u09AC\u09BE\u0987\u09B0\u09C7\u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\
  \u09B0\u09C7\u09B0\u09BF\u0997\u09C1\u09B2\u09BF\u09B0 \u09AA\u09CD\u09B0\u09AF\u09BC\
  \u09CB\u099C\u09A8 \u099B\u09BE\u09A1\u09BC\u09BE\u0987\u0964 \u0986\u09AA\u09A8\
  \u09BF \u098F\u099F\u09BF \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u0995\u09B0\u09AC\
  \u09C7\u09A8 \u09A4\u09BE \u098F\u0996\u09BE\u09A8\u09C7."
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\u09A7\
  \u09BE\u09A8 \u098F\u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\u09A5\
  \u09BE\u09AA\u09A8"
weight: 10
---

## কিভাবে:
Dart তার `String` ক্লাসের মাধ্যমে সরাসরি টেক্সট খোঁজা এবং প্রতিস্থাপনের জন্য শক্তিশালী পদ্ধতি প্রদান করে, বাইরের লাইব্রেরিগুলির প্রয়োজন ছাড়াই। আপনি এটি কিভাবে করবেন তা এখানে:

### মৌলিক খোঁজা ও প্রতিস্থাপন
একটি সাবস্ট্রিং খুঁজে তা অন্য একটি স্ট্রিং দ্বারা প্রতিস্থাপন করতে, আপনি `replaceAll` ব্যবহার করতে পারেন:

```dart
String sampleText = "Hello, Dart! Dart is great.";
String modifiedText = sampleText.replaceAll("Dart", "Flutter");
print(modifiedText); // আউটপুট: Hello, Flutter! Flutter is great.
```

### নিয়মিত প্রকাশের ব্যবহার
আরও জটিল খোঁজা ও প্রতিস্থাপনের চাহিদা মেটাতে, Dart টেক্সটে প্যাটার্ন মিলানো ও প্রতিস্থাপনের জন্য `RegExp` ক্লাসের মাধ্যমে নিয়মিত প্রকাশ ব্যবহার করে।

```dart
String sampleText = "Dart 2023, Flutter 2023";
String modifiedText = sampleText.replaceAll(RegExp(r'\d+'), "2024");
print(modifiedText); // আউটপুট: Dart 2024, Flutter 2024
```

এই উদাহরণটি স্ট্রিংয়ে এক বা একাধিক সংখ্যার সকল ঘটনা (`\d+`) খুঁজে পায় এবং "2024" এর সাথে প্রতিস্থাপন করে।

### কেস-অসংবেদী খোঁজা
একটি কেস-অসংবেদী অনুসন্ধান করতে, আপনি `RegExp` নির্মাতাকে কেস উপেক্ষা করতে পরিবর্তন করতে পারেন:

```dart
String sampleText = "Welcome to Dart, the programming language.";
String modifiedText = sampleText.replaceAll(RegExp(r'dart', caseSensitive: false), "Flutter");
print(modifiedText); // আউটপুট: Welcome to Flutter, the programming language.
```

### একটি ফাংশনের সাথে প্রতিস্থাপন
মিলের স্বয়ং উপর ভিত্তি করে গতিশীল প্রতিস্থাপনের জন্য, Dart `replaceAllMapped` এ একটি ফাংশন পাস করতে সমর্থন করে। এই ফাংশনটি মিলানো ক্রমগুলিতে অপারেশন বা গণনা করতে পারে:

```dart
String sampleText = "Increment 5 by 1 to get 6.";
String incrementedText = sampleText.replaceAllMapped(RegExp(r'\d+'), (Match m) => (int.parse(m[0]!) + 1).toString());
print(incrementedText); // আউটপুট: Increment 6 by 1 to get 7.
```

এটি প্রতিটি সংখ্যার ক্রমকে এর বৃদ্ধি মানের সাথে প্রতিস্থাপন করে। প্রতিটি মিলানো একটি পূর্ণসংখ্যায় পার্স করা হয়, বৃদ্ধি করা হয়, এবং তারপর প্রতিস্থাপনের জন্য একটি স্ট্রিংয়ে পরিণত করা হয়।

Dart এর স্ট্রিং নিয়ন্ত্রণের ক্ষমতা, বিশেষত টেক্সট খোঁজা এবং প্রতিস্থাপন করা, এটিকে আপনার অ্যাপ্লিকেশনের ভেতরে ডেটা প্রসেসিং এবং প্রস্তুতির জন্য একটি শক্তিশালী টুল করে তোলে। সরল স্ট্রিং প্রতিস্থাপন ব্যবহার করে বা নিয়মিত প্রকাশের শক্তি কাজে লাগিয়ে, Dart কার্যকর টেক্সট নিয়ন্ত্রণের জন্য প্রয়োজনীয় নমনীয়তা এবং পারফরম্যান্স প্রদান করে।

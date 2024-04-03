---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:24:40.959657-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09A1\u09BE\u09B0\u09CD\u099F\
  \ \u09AE\u09CD\u09AF\u09BE\u09AA \u09A4\u09C8\u09B0\u09BF \u098F\u09AC\u0982 \u0985\
  \u09AA\u09BE\u09B0\u09C7\u09B6\u09A8 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u098F\u0995\u099F\u09BF \u09B8\u09CB\u099C\u09BE \u09B8\u09CB\u099C\u09BE\
  \ \u09B8\u09BF\u09A8\u099F\u09CD\u09AF\u09BE\u0995\u09CD\u09B8 \u09AA\u09CD\u09B0\
  \u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964 \u09A8\u09BF\u099A\u09C7 \u09AE\u09CD\
  \u09AF\u09BE\u09AA \u09A4\u09C8\u09B0\u09BF, \u0989\u09AA\u09BE\u09A6\u09BE\u09A8\
  \ \u09AF\u09CB\u0997 \u098F\u09AC\u0982 \u09AE\u09BE\u09A8 \u0986\u09A8\u09A4\u09C7\
  \ \u09AE\u09CC\u09B2\u09BF\u0995 \u0985\u09AA\u09BE\u09B0\u09C7\u09B6\u09A8\u09C7\
  \u09B0 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3\u2026"
lastmod: '2024-03-17T18:47:43.710266-06:00'
model: gpt-4-0125-preview
summary: "\u09A1\u09BE\u09B0\u09CD\u099F \u09AE\u09CD\u09AF\u09BE\u09AA \u09A4\u09C8\
  \u09B0\u09BF \u098F\u09AC\u0982 \u0985\u09AA\u09BE\u09B0\u09C7\u09B6\u09A8 \u0995\
  \u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF \u09B8\u09CB\
  \u099C\u09BE \u09B8\u09CB\u099C\u09BE \u09B8\u09BF\u09A8\u099F\u09CD\u09AF\u09BE\
  \u0995\u09CD\u09B8 \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964\
  \ \u09A8\u09BF\u099A\u09C7 \u09AE\u09CD\u09AF\u09BE\u09AA \u09A4\u09C8\u09B0\u09BF\
  , \u0989\u09AA\u09BE\u09A6\u09BE\u09A8 \u09AF\u09CB\u0997 \u098F\u09AC\u0982 \u09AE\
  \u09BE\u09A8 \u0986\u09A8\u09A4\u09C7 \u09AE\u09CC\u09B2\u09BF\u0995 \u0985\u09AA\
  \u09BE\u09B0\u09C7\u09B6\u09A8\u09C7\u09B0 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2."
title: "\u098F\u09B8\u09CB\u09B8\u09BF\u09AF\u09BC\u09C7\u099F\u09BF\u09AD \u0985\u09CD\
  \u09AF\u09BE\u09B0\u09C7\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0"
weight: 15
---

## কিভাবে:
ডার্ট ম্যাপ তৈরি এবং অপারেশন করার জন্য একটি সোজা সোজা সিনট্যাক্স প্রদান করে। নিচে ম্যাপ তৈরি, উপাদান যোগ এবং মান আনতে মৌলিক অপারেশনের উদাহরণ দেওয়া হল:

```dart
void main() {
  // একটি ম্যাপ তৈরি করা
  var fruitColors = {
    'apple': 'red',
    'banana': 'yellow',
    'grape': 'purple'
  };

  // একটি নতুন কী-ভ্যালু জোড়া যোগ করা
  fruitColors['orange'] = 'orange';

  // এর কী দ্বারা একটি মান অ্যাক্সেস করা
  print(fruitColors['apple']); // আউটপুট: red

  // একটি মান আপডেট করা
  fruitColors['banana'] = 'green';

  // ম্যাপের উপরে ইতারেট করা
  fruitColors.forEach((fruit, color) {
    print('$fruit: $color');
  });
  // নমুনা আউটপুট:
  // apple: red
  // banana: green
  // grape: purple
  // orange: orange
}
```

জটিল ডেটা স্ট্রাকচার বা বর্ধিত ফাংশনালিটির জন্য, ডার্ট প্রোগ্রামাররা প্রায়ই অতিরিক্ত লাইব্রেরিগুলির উপর নির্ভর করেন। এমন একটি লাইব্রেরি হল `collection` যা উন্নত সংগ্রহ ধরন এবং ইউটিলিটি প্রদান করে। যদিও `collection` ম্যাপগুলি সর্বনিম্নভাবে পরিচালনা করার মূল উপায়টি পরিবর্তন করে না, এটি ইউটিলিটি ফাংশন এবং আরো জটিল সংগ্রহ ধরনের সাথে তাদের সমৃদ্ধ করে। এখানে আপনি এটি কীভাবে ব্যবহার করতে পারেন যেমন মান অনুসারে একটি ম্যাপ সর্টিং করার একটি নির্দিষ্ট কার্য জন্য:

প্রথমে, নিশ্চিত করুন `collection` প্যাকেজটি আপনার `pubspec.yaml` ফাইলে অন্তর্ভুক্ত করা আছে:

```yaml
dependencies:
  collection: ^1.15.0
```

তারপর, আপনি এটি নিম্নরূপে ব্যবহার করতে পারেন:

```dart
import 'package:collection/collection.dart';

void main() {
  var fruitColors = {
    'apple': 'red',
    'banana': 'yellow',
    'grape': 'purple',
    'orange': 'orange'
  };

  // ম্যাপটি এর মানের (রংগুলির) অনুসারে সর্ট করা
  var sortedFruitsByColor = SplayTreeMap.from(
    fruitColors,
    (key1, key2) => fruitColors[key1]!.compareTo(fruitColors[key2]!)
  );

  print(sortedFruitsByColor);
  // আউটপুট:
  // {orange: orange, apple: red, banana: yellow, grape: purple}
}
```

এই উদাহরণটি প্রদর্শন করে কীভাবে একটি ম্যাপের এন্ট্রিগুলি তাদের মানের উপর ভিত্তি করে সর্টিং করা হয়, ডার্ট এবং এর জীবন্ত ইকোসিস্টেম কীভাবে অ্যাসোসিয়েটিভ অ্যারেগু Samsung

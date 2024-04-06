---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:05:24.462633-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Dart \u098F\u09B0 \u0995\u09CB\
  \u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF `DateTime` \u0995\u09CD\
  \u09B2\u09BE\u09B8\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09A4\
  \u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE \u09B8\
  \u09B9\u099C \u0995\u09B0\u09C7\u0964 \u09B8\u09CB\u099C\u09BE\u09B8\u09BE\u09AA\
  \u099F\u09BE \u0995\u09CD\u09B7\u09C7\u09A4\u09CD\u09B0\u09C7, \u09AF\u09C7\u0996\
  \u09BE\u09A8\u09C7 \u0986\u09AA\u09A8\u09BF \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\
  \u09B0 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09AF\u09BC\u09C7\u09B0 \u09AB\
  \u09B0\u09AE\u09CD\u09AF\u09BE\u099F \u099C\u09BE\u09A8\u09C7\u09A8, \u0986\u09AA\
  \u09A8\u09BF\u2026"
lastmod: '2024-04-05T21:53:51.845583-06:00'
model: gpt-4-0125-preview
summary: "Dart \u098F\u09B0 \u0995\u09CB\u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\
  \u09C7\u09B0\u09BF `DateTime` \u0995\u09CD\u09B2\u09BE\u09B8\u09C7\u09B0 \u09AE\u09BE\
  \u09A7\u09CD\u09AF\u09AE\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u09B0\
  \u09CD\u09B8 \u0995\u09B0\u09BE \u09B8\u09B9\u099C \u0995\u09B0\u09C7\u0964 \u09B8\
  \u09CB\u099C\u09BE\u09B8\u09BE\u09AA\u099F\u09BE \u0995\u09CD\u09B7\u09C7\u09A4\u09CD\
  \u09B0\u09C7, \u09AF\u09C7\u0996\u09BE\u09A8\u09C7 \u0986\u09AA\u09A8\u09BF \u09A4\
  \u09BE\u09B0\u09BF\u0996\u09C7\u09B0 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09AF\
  \u09BC\u09C7\u09B0 \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F \u099C\u09BE\u09A8\
  \u09C7\u09A8, \u0986\u09AA\u09A8\u09BF `DateTime.parse()` \u09AE\u09C7\u09A5\u09A1\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\
  \u09B0\u09C7\u09A8\u0964 \u09A4\u09AC\u09C7, \u0986\u09B0\u0993 \u099C\u099F\u09BF\
  \u09B2 \u09AA\u09B0\u09BF\u09B8\u09CD\u09A5\u09BF\u09A4\u09BF \u09AC\u09BE \u098F\
  \u0995\u09BE\u09A7\u09BF\u0995 \u09AB\u09B0\u09CD\u09AE\u09CD\u09AF\u09BE\u099F\u09C7\
  \u09B0 \u09B8\u09BE\u09A5\u09C7 \u09A1\u09BF\u09B2 \u0995\u09B0\u09BE\u09B0 \u0995\
  \u09CD\u09B7\u09C7\u09A4\u09CD\u09B0\u09C7, `intl` \u09AA\u09CD\u09AF\u09BE\u0995\
  \u09C7\u099C, \u09AC\u09BF\u09B6\u09C7\u09B7\u09A4 `DateFormat` \u0995\u09CD\u09B2\
  \u09BE\u09B8, \u0985\u09AE\u09C2\u09B2\u09CD\u09AF \u09B9\u09AF\u09BC\u09C7 \u0993\
  \u09A0\u09C7\u0964."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A4\
  \u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 30
---

## কিভাবে:
Dart এর কোর লাইব্রেরি `DateTime` ক্লাসের মাধ্যমে তারিখ পার্স করা সহজ করে। সোজাসাপটা ক্ষেত্রে, যেখানে আপনি তারিখের স্ট্রিংয়ের ফরম্যাট জানেন, আপনি `DateTime.parse()` মেথড ব্যবহার করতে পারেন। তবে, আরও জটিল পরিস্থিতি বা একাধিক ফর্ম্যাটের সাথে ডিল করার ক্ষেত্রে, `intl` প্যাকেজ, বিশেষত `DateFormat` ক্লাস, অমূল্য হয়ে ওঠে।

### Dart কোর লাইব্রেরি ব্যবহার করে:
```dart
void main() {
  // DateTime.parse() ব্যবহার করে
  var dateString = "2023-10-31";
  var parsedDate = DateTime.parse(dateString);
  
  print(parsedDate); // 2023-10-31 00:00:00.000
}
```

### `intl` প্যাকেজ ব্যবহার করে:
প্রথমে, আপনার `pubspec.yaml` ফাইলে `intl` প্যাকেজ যোগ করুন:
```yaml
dependencies:
  intl: ^0.17.0
```
তারপর, প্যাকেজটি ইম্পোর্ট করুন এবং পার্স করার জন্য `DateFormat` ব্যবহার করুন:
```dart
import 'package:intl/intl.dart';

void main() {
  var dateString = "October 31, 2023";
  var dateFormat = DateFormat("MMMM dd, yyyy");
  var parsedDate = dateFormat.parse(dateString);
  
  print(parsedDate); // 2023-10-31 00:00:00.000
}
```
`intl` প্যাকেজটি তারিখ পার্সিংয়ের জন্য শক্তিশালী বিকল্পগুলি অফার করে, বিভিন্ন আন্তর্জাতিক তারিখ ফরম্যাটগুলিকে সহজে হ্যান্ডল করতে সাহায্য করে।

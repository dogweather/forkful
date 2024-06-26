---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:05.091686-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Dart \u098F\u09A4\u09C7 `dart:developer`\
  \ \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u09B0 \u09AE\u09BE\u09A7\
  \u09CD\u09AF\u09AE\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09B9\u099C \u09B2\u0997\
  \u09BF\u0982 \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE \u0985\
  \u09A8\u09CD\u09A4\u09B0\u09CD\u09AD\u09C1\u0995\u09CD\u09A4 \u09B0\u09AF\u09BC\u09C7\
  \u099B\u09C7\u0964 \u0986\u09B0\u09CB \u099C\u099F\u09BF\u09B2 \u09B2\u0997\u09BF\
  \u0982 \u099A\u09BE\u09B9\u09BF\u09A6\u09BE\u0997\u09C1\u09B2\u09BF\u09B0 \u099C\
  \u09A8\u09CD\u09AF, \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\
  \u09B0\u09B0\u09BE \u09AA\u09CD\u09B0\u09BE\u09AF\u09BC\u0987\u2026"
lastmod: '2024-04-05T21:53:51.840034-06:00'
model: gpt-4-0125-preview
summary: "Dart \u098F\u09A4\u09C7 `dart:developer` \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\
  \u09C7\u09B0\u09BF\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u098F\u0995\
  \u099F\u09BF \u09B8\u09B9\u099C \u09B2\u0997\u09BF\u0982 \u09AA\u09CD\u09B0\u0995\
  \u09CD\u09B0\u09BF\u09AF\u09BC\u09BE \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09AD\u09C1\
  \u0995\u09CD\u09A4 \u09B0\u09AF\u09BC\u09C7\u099B\u09C7\u0964 \u0986\u09B0\u09CB\
  \ \u099C\u099F\u09BF\u09B2 \u09B2\u0997\u09BF\u0982 \u099A\u09BE\u09B9\u09BF\u09A6\
  \u09BE\u0997\u09C1\u09B2\u09BF\u09B0 \u099C\u09A8\u09CD\u09AF, \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09AA\u09CD\u09B0\u09BE\
  \u09AF\u09BC\u0987 \u09A5\u09BE\u09B0\u09CD\u09A1-\u09AA\u09BE\u09B0\u09CD\u099F\
  \u09BF \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u0997\u09C1\u09B2\u09BF\
  \ \u09AF\u09C7\u09AE\u09A8 `logger` \u098F\u09AC\u0982 `log4dart`-\u098F \u09AE\u09CB\
  \u09A1\u09BC \u09A8\u09C7\u09AF\u09BC\u0964."
title: "\u09B2\u0997\u09BF\u0982"
weight: 17
---

## কিভাবে:
Dart এতে `dart:developer` লাইব্রেরির মাধ্যমে একটি সহজ লগিং প্রক্রিয়া অন্তর্ভুক্ত রয়েছে। আরো জটিল লগিং চাহিদাগুলির জন্য, প্রোগ্রামাররা প্রায়ই থার্ড-পার্টি লাইব্রেরিগুলি যেমন `logger` এবং `log4dart`-এ মোড় নেয়।

### `dart:developer` ব্যবহার করে
এটি মূল লগিংয়ের জন্য উপযুক্ত, বিশেষ করে উন্নয়নের সময়:

```dart
import 'dart:developer';

void main() {
  log('এটি একটি ডিবাগ লগ মেসেজ।');
}
```

আউটপুট:
```
এটি একটি ডিবাগ লগ মেসেজ।
```

### `logger` প্যাকেজ ব্যবহার করে
একটি আরো সম্পূর্ণ সমাধানের জন্য, `logger` প্যাকেজ বিভিন্ন মাত্রার লগিং (যেমন, তথ্য, সতর্কবার্তা, ত্রুটি) অফার করে এবং এটি আরো পড়ায় সহজ মানেরে ফর্ম্যাট করা যায়।

প্রথমে, আপনার `pubspec.yaml` ফাইলে `logger` ডিপেনডেন্সি যোগ করুন:

```yaml
dependencies:
  logger: ^1.0.0
```

তারপর, এটি নিম্নরূপ ব্যবহার করুন:

```dart
import 'package:logger/logger.dart';

var logger = Logger();

void main() {
  logger.d("এটি একটি ডিবাগ মেসেজ");
  logger.w("এটি একটি সতর্কবার্তা মেসেজ");
  logger.e("এটি একটি ত্রুটি মেসেজ");
}
```

নমুনা আউটপুট এই রকম দেখাবে, প্রতিটি মেসেজের ধরণ সহজ শনাক্তকরণের জন্য ভিন্নভাবে ফরম্যাট করা:

```
💬 এটি একটি ডিবাগ মেসেজ
⚠️ এটি একটি সতর্কবার্তা মেসেজ
❗️ এটি একটি ত্রুটি মেসেজ
```

### `log4dart` প্যাকেজ ব্যবহার করে
যেসব অ্যাপ্লিকেশনের কনফিগারেশন ভিত্তিক লগিংয়ের (Log4j-এর মতো) প্রয়োজন হয়, সেজন্য `log4dart` একটি পরিচিত পদ্ধতি অফার করে। এটি বিশেষত বৃহত মাপের অ্যাপ্লিকেশনগুলির জন্য সুবিধাজনক।

নিশ্চিত করুন যে আপনি `pubspec.yaml`-এ `log4dart` অন্তর্ভুক্ত করেছেন:

```yaml
dependencies:
  log4dart: ^2.0.0
```

একটি সহজ ব্যবহারের উদাহরণ:

```dart
import 'package:log4dart/log4dart.dart';

void main() {
  final logger = LoggerFactory.getLogger("MyApp");
  logger.debug("MyApp ডিবাগ করা হচ্ছে");
  logger.info("তথ্যমূলক বার্তা");
}
```

আউটপুট:

```
DEBUG: MyApp ডিবাগ করা হচ্ছে
INFO: তথ্যমূলক বার্তা
```

এই পদ্ধতিগুলি প্রতিটি বিভিন্ন মাত্রার নমনীয়তা এবং জটিলতা প্রদান করে, সহজ ডিবাগিং বার্তা থেকে শুরু করে জটিল অ্যাপ্লিকেশনের চাহিদাগুলিকে পূরণ করার জন্য সম্পূর্ণ কনফিগুরাবল লগিং পর্যন্ত।

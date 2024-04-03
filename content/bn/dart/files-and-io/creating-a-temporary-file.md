---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:01.322221-06:00
description: "\u0995\u09C0\u09AD\u09BE\u09AC\u09C7: \u09A1\u09BE\u09B0\u09CD\u099F\
  \u09C7\u09B0 `dart:io` \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u0985\
  \u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0 \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\
  \u09B0\u09BF \u0995\u09B0\u09BE\u09B0 \u09B8\u09C1\u09AF\u09CB\u0997 \u09AA\u09CD\
  \u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7 `Directory` \u0995\u09CD\u09B2\u09BE\
  \u09B8\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7\u0964 \u098F\u0996\
  \u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u0985\u09B8\u09CD\u09A5\u09BE\u09AF\
  \u09BC\u09C0 \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u098F\u09AC\u0982\
  \ \u09A4\u09BE\u09A4\u09C7 \u0995\u09BF\u099B\u09C1\u2026"
lastmod: '2024-03-17T18:47:43.740401-06:00'
model: gpt-4-0125-preview
summary: "\u09A1\u09BE\u09B0\u09CD\u099F\u09C7\u09B0 `dart:io` \u09B2\u09BE\u0987\u09AC\
  \u09CD\u09B0\u09C7\u09B0\u09BF \u0985\u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0\
  \ \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE\u09B0 \u09B8\
  \u09C1\u09AF\u09CB\u0997 \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\
  \ `Directory` \u0995\u09CD\u09B2\u09BE\u09B8\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\
  \u09AF\u09AE\u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF\
  \ \u0985\u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0 \u09AB\u09BE\u0987\u09B2 \u09A4\
  \u09C8\u09B0\u09BF \u098F\u09AC\u0982 \u09A4\u09BE\u09A4\u09C7 \u0995\u09BF\u099B\
  \u09C1 \u0995\u09A8\u09CD\u099F\u09C7\u09A8\u09CD\u099F \u09B2\u09C7\u0996\u09BE\
  \u09B0 \u098F\u0995\u099F\u09BF \u09B8\u09B9\u099C \u0989\u09AA\u09BE\u09AF\u09BC\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2."
title: "\u098F\u0995\u099F\u09BF \u0985\u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0\
  \ \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE"
weight: 21
---

## কীভাবে:
ডার্টের `dart:io` লাইব্রেরি অস্থায়ী ফাইল তৈরি করার সুযোগ প্রদান করে `Directory` ক্লাসের মাধ্যমে। এখানে একটি অস্থায়ী ফাইল তৈরি এবং তাতে কিছু কন্টেন্ট লেখার একটি সহজ উপায় দেওয়া হল:

```dart
import 'dart:io';

Future<void> main() async {
  // একটি অস্থায়ী ডিরেক্টরি তৈরি করুন (সিস্টেম-নির্দিষ্ট অবস্থান)
  Directory tempDir = await Directory.systemTemp.createTemp('my_temp_dir_');

  // ঐ ডিরেক্টরিতে একটি অস্থায়ী ফাইল তৈরি করুন
  File tempFile = File('${tempDir.path}/my_temp_file.txt');

  // অস্থায়ী ফাইলে কিছু কন্টেন্ট লিখুন
  await tempFile.writeAsString('This is some temporary content');

  print('Temporary file created: ${tempFile.path}');

  // নমুনা আউটপুট: Temporary file created: /tmp/my_temp_dir_A1B2C3/my_temp_file.txt
}
```

### তৃতীয় পক্ষ্যের লাইব্রেরি ব্যবহার: `path_provider`
বিশেষত মোবাইল অ্যাপ্লিকেশনগুলির (Flutter সহ) জন্য, আপনি হয়তো আরও সহজ এবং পরিচালনাযোগ্য উপায়ে অস্থায়ী ফাইল তৈরি করতে চান। `path_provider` প্যাকেজটি বিভিন্ন প্ল্যাটফর্মে (iOS, Android, ইত্যাদি) সঠিক অস্থায়ী ডিরেক্টরি খুঁজে পেতে আপনাকে সাহায্য করতে পারে।

প্রথমে, `path_provider` কে আপনার `pubspec.yaml` এর নির্ভরতাগুলির অধীনে যোগ করুন:

```yaml
dependencies:
  path_provider: ^2.0.9
```

এবং এখানে দেখানো হল কিভাবে আপনি এটি দিয়ে একটি অস্থায়ী ফাইল তৈরি করতে পারেন:

```dart
import 'dart:io';
import 'package:path_provider/path_provider.dart';

Future<void> main() async {
  // অস্থায়ী ডিরেক্টরি পেতে
  final Directory tempDir = await getTemporaryDirectory();

  // ঐ ডিরেক্টরিতে একটি অস্থায়ী ফাইল তৈরি করুন
  final File tempFile = File('${tempDir.path}/my_temp_file.txt');

  // অস্থায়ী ফাইলে কিছু কন্টেন্ট লিখুন
  await tempFile.writeAsString('This is some temporary content with path_provider');

  print('Temporary file created with path_provider: ${tempFile.path}');

  // নমুনা আউটপুট: Temporary file created with path_provider: /tmp/my_temp_file.txt (প্ল্যাটফর্মের অনুযায়ী পথ পরিবর্তনশীল)
}
```

এই স্নিপেটগুলি ডার্টে অস্থায়ী ফাইল তৈরি এবং তার সাথে মিথস্ক্রিয়া করার প্রক্রিয়াকে ইঙ্গিত দেয়, যা ডেটা ব্যবস্থাপনার জন্য একটি সরল এবং বাস্তবসম্মত উপায় প্রদান করে।

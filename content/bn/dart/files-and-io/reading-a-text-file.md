---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:08:15.779970-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09A1\u09BE\u09B0\u09CD\u099F\
  \u09C7\u09B0 \u0995\u09CB\u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\
  \u09BF, `dart:io`, \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2\
  \ \u09B8\u09BF\u0999\u09CD\u0995\u09CD\u09B0\u09CB\u09A8\u09BE\u09B8\u09B2\u09BF\
  \ \u0985\u09A5\u09AC\u09BE \u0985\u09CD\u09AF\u09BE\u09B8\u09BF\u0999\u09CD\u0995\
  \u09CD\u09B0\u09CB\u09A8\u09BE\u09B8\u09B2\u09BF \u09AA\u09A1\u09BC\u09BE\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8\u09C0\
  \u09AF\u09BC \u0995\u09BE\u09B0\u09CD\u09AF\u0995\u09CD\u09B0\u09AE \u09B8\u09B0\
  \u09AC\u09B0\u09BE\u09B9 \u0995\u09B0\u09C7\u0964 \u0989\u09AD\u09AF\u09BC \u0989\
  \u09AA\u09BE\u09AF\u09BC\u09C7 \u098F\u099F\u09BF\u2026"
lastmod: '2024-04-05T21:53:51.861713-06:00'
model: gpt-4-0125-preview
summary: "\u09A1\u09BE\u09B0\u09CD\u099F\u09C7\u09B0 \u0995\u09CB\u09B0 \u09B2\u09BE\
  \u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF, `dart:io`, \u099F\u09C7\u0995\u09CD\u09B8\
  \u099F \u09AB\u09BE\u0987\u09B2 \u09B8\u09BF\u0999\u09CD\u0995\u09CD\u09B0\u09CB\
  \u09A8\u09BE\u09B8\u09B2\u09BF \u0985\u09A5\u09AC\u09BE \u0985\u09CD\u09AF\u09BE\
  \u09B8\u09BF\u0999\u09CD\u0995\u09CD\u09B0\u09CB\u09A8\u09BE\u09B8\u09B2\u09BF \u09AA\
  \u09A1\u09BC\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09AA\u09CD\u09B0\u09AF\u09BC\
  \u09CB\u099C\u09A8\u09C0\u09AF\u09BC \u0995\u09BE\u09B0\u09CD\u09AF\u0995\u09CD\u09B0\
  \u09AE \u09B8\u09B0\u09AC\u09B0\u09BE\u09B9 \u0995\u09B0\u09C7\u0964 \u0989\u09AD\
  \u09AF\u09BC \u0989\u09AA\u09BE\u09AF\u09BC\u09C7 \u098F\u099F\u09BF \u0995\u09BF\
  \u09AD\u09BE\u09AC\u09C7 \u0995\u09B0\u09A4\u09C7 \u09B9\u09AF\u09BC \u09A4\u09BE\
  \u09B0 \u0989\u09AA\u09B0\u09C7 \u098F\u0995\u099F\u09BF \u09A6\u09C3\u09B7\u09CD\
  \u099F\u09BF\u09AA\u09BE\u09A4\u0964 **\u09B8\u09BF\u0999\u09CD\u0995\u09CD\u09B0\
  \u09CB\u09A8\u09BE\u09B8\u09B2\u09BF:**."
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\
  \u09BC\u09BE"
weight: 22
---

## কিভাবে:
ডার্টের কোর লাইব্রেরি, `dart:io`, টেক্সট ফাইল সিঙ্ক্রোনাসলি অথবা অ্যাসিঙ্ক্রোনাসলি পড়ার জন্য প্রয়োজনীয় কার্যক্রম সরবরাহ করে। উভয় উপায়ে এটি কিভাবে করতে হয় তার উপরে একটি দৃষ্টিপাত।

**সিঙ্ক্রোনাসলি:**

```dart
import 'dart:io';

void main() {
  var fileName = "path/to/your/textfile.txt";
  var file = File(fileName);

  // ফাইলটি সিঙ্ক্রোনাসলি পড়া হচ্ছে
  var contents;
  try {
    contents = file.readAsStringSync();
    print(contents);
  } catch (e) {
    print('ফাইল পড়ার সময় ত্রুটি: $e');
  }
}
```

**অ্যাসিঙ্ক্রোনাসলি:**

বিশেষ করে বড় ফাইল অথবা রেসপন্সিভ অ্যাপ্লিকেশনগুলির ক্ষেত্রে ফাইলটি পড়া হওয়ার সময় প্রোগ্রামটি ব্লক হওয়া এড়াতে:

```dart
import 'dart:io';

void main() async {
  var fileName = "path/to/your/textfile.txt";
  var file = File(fileName);

  try {
    String contents = await file.readAsString();
    print(contents);
  } catch (e) {
    print('ফাইল পড়ার সময় ত্রুটি: $e');
  }
}
```

**নমুনা আউটপুট:**

যদি আপনার টেক্সট ফাইলটি ধারণ করে:

```
Hello, Dart!
```

উপরের উভয় পদ্ধতিই আউটপুট দেবে:

```
Hello, Dart!
```

**থার্ড-পার্টি লাইব্রেরি ব্যবহার:**

সাধারণীকৃত ফাইল অপারেশন বা উন্নত ত্রুটি হ্যান্ডলিংয়ের মতো অতিরিক্ত বৈশিষ্ট্যের জন্য, আপনি থার্ড-পার্টি লাইব্রেরি যেমন `package:file` বিবেচনা করতে পারেন। তবে, আমার শেষ আপডেট অনুযায়ী, উপরে দেখানো হয়েছে যেমন, সরাসরি `dart:io` প্যাকেজ ব্যবহার করেই ডার্টে টেক্সট ফাইল পড়ার সবচেয়ে সাধারণ এবং সরল পদ্ধতি।

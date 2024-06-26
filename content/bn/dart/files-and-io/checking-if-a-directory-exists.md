---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:41.632644-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Dart \u09AB\u09BE\u0987\u09B2\
  \ \u098F\u09AC\u0982 \u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF\u09B0\
  \ \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09A4\u09C7 `dart:io`\
  \ \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09C0 \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\u0964 \u098F\u0995\u099F\u09BF \u09A1\u09BF\
  \u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF\u09B0 \u0985\u09B8\u09CD\u09A4\u09BF\u09A4\
  \u09CD\u09AC \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\u09BE \u098F\
  \u0995\u099F\u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3 \u0989\u09AA\u09BE\u09AF\
  \u09BC \u09B9\u09B2\u09CB."
lastmod: '2024-03-17T18:47:43.735201-06:00'
model: gpt-4-0125-preview
summary: "Dart \u09AB\u09BE\u0987\u09B2 \u098F\u09AC\u0982 \u09A1\u09BF\u09B0\u09C7\
  \u0995\u09CD\u099F\u09B0\u09BF\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C\
  \ \u0995\u09B0\u09A4\u09C7 `dart:io` \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\
  \u09C0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\u0964 \u098F\
  \u0995\u099F\u09BF \u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF\u09B0\
  \ \u0985\u09B8\u09CD\u09A4\u09BF\u09A4\u09CD\u09AC \u09AA\u09B0\u09C0\u0995\u09CD\
  \u09B7\u09BE \u0995\u09B0\u09BE \u098F\u0995\u099F\u09BF \u09B8\u09BE\u09A7\u09BE\
  \u09B0\u09A3 \u0989\u09AA\u09BE\u09AF\u09BC \u09B9\u09B2\u09CB."
title: "\u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF \u0986\u099B\u09C7\
  \ \u0995\u09BF\u09A8\u09BE \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\
  \u09BE"
weight: 20
---

## কিভাবে:
Dart ফাইল এবং ডিরেক্টরির সাথে কাজ করতে `dart:io` লাইব্রেরী ব্যবহার করে। একটি ডিরেক্টরির অস্তিত্ব পরীক্ষা করা একটি সাধারণ উপায় হলো:

```dart
import 'dart:io';

void main() {
  var directory = Directory('path/to/your/directory');

  if (directory.existsSync()) {
    print('Directory exists');
  } else {
    print('Directory does not exist');
  }
}
```
যদি ডিরেক্টরি অস্তিত্বে থাকে তবে নমুনা আউটপুট:
```
Directory exists
```

অথবা, যদি না থাকে:
```
Directory does not exist
```

আরও জটিল পরিস্থিতি, যেমন অ্যাসিঙ্ক্রোনাসলি চেক করা বা যদি ডিরেক্টরি না থাকে তবে তৈরী করা, সামলানোর জন্য নিম্নলিখিত পদ্ধতি ব্যবহার করা যেতে পারে:

```dart
import 'dart:io';

void main() async {
  var directory = Directory('path/to/your/directory');

  // অ্যাসিঙ্ক্রোনাসলি চেক করুন যে ডিরেক্টরির অস্তিত্ব আছে কিনা
  var exists = await directory.exists();
  if (exists) {
    print('Directory exists');
  } else {
    print('Directory does not exist, creating...');
    await directory.create(); // এটি ডিরেক্টরিটি তৈরী করে
    print('Directory created');
  }
}
```

যদি ডিরেক্টরি অস্তিত্বে না থাকে এবং তৈরি করা হয় তবে নমুনা আউটপুট:
```
Directory does not exist, creating...
Directory created
```

ফাইল এবং ডিরেক্টরিগুলি পরিচালনার জন্য Dart-এর অন্তর্নির্মিত ক্ষমতাগুলি সাধারণত পর্যাপ্ত থাকে, তাই এই কাজের জন্য তৃতীয় পক্ষের লাইব্রেরি সাধারণত প্রয়োজন হয় না। তবে, আরও জটিল ফাইল সিস্টেম অপারেশনের জন্য, `path` মত প্যাকেজগুলি (একটি প্ল্যাটফর্ম-নিরপেক্ষ উপায়ে পাথগুলি পরিবর্তন করার জন্য) `dart:io` লাইব্রেরীকে পরিপূরক করতে পারে কিন্তু প্রদর্শিত তুলনায় আরও উন্নত ডিরেক্টরি অস্তিত্ব চেকগুলি সরাসরি অফার করে না।

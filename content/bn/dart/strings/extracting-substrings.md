---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:54.126474-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Dart-\u098F, \u0986\u09AA\u09A8\
  \u09BF \u09AC\u09BF\u09AD\u09BF\u09A8\u09CD\u09A8 \u09AA\u09A6\u09CD\u09A7\u09A4\
  \u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09B8\u09BE\
  \u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u0995\u09CD\u09B8\u099F\u09CD\
  \u09B0\u09BE\u0995\u09CD\u099F \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\
  \u09A8, \u09AF\u09C7\u09AE\u09A8 `substring()`, `split()`, \u098F\u09AC\u0982 \u09B0\
  \u09C7\u0997\u09C1\u09B2\u09BE\u09B0 \u098F\u0995\u09CD\u09B8\u09AA\u09CD\u09B0\u09C7\
  \u09B6\u09A8\u0964 \u09AA\u09CD\u09B0\u09A4\u09BF\u099F\u09BF \u09AA\u09A6\u09CD\
  \u09A7\u09A4\u09BF\u2026"
lastmod: '2024-04-05T21:53:51.805266-06:00'
model: gpt-4-0125-preview
summary: "Dart-\u098F, \u0986\u09AA\u09A8\u09BF \u09AC\u09BF\u09AD\u09BF\u09A8\u09CD\
  \u09A8 \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09C7 \u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u098F\u0995\u09CD\u09B8\u099F\u09CD\u09B0\u09BE\u0995\u09CD\u099F \u0995\
  \u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8, \u09AF\u09C7\u09AE\u09A8 `substring()`,\
  \ `split()`, \u098F\u09AC\u0982 \u09B0\u09C7\u0997\u09C1\u09B2\u09BE\u09B0 \u098F\
  \u0995\u09CD\u09B8\u09AA\u09CD\u09B0\u09C7\u09B6\u09A8\u0964 \u09AA\u09CD\u09B0\u09A4\
  \u09BF\u099F\u09BF \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF \u09AC\u09BF\u09AD\u09BF\
  \u09A8\u09CD\u09A8 \u0989\u09A6\u09CD\u09A6\u09C7\u09B6\u09CD\u09AF\u09C7 \u09B8\
  \u09C7\u09AC\u09BE \u0995\u09B0\u09C7 \u098F\u09AC\u0982 \u09B8\u09CD\u099F\u09CD\
  \u09B0\u09BF\u0982 \u09B9\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09B2\u09BF\u0982\
  -\u098F \u09A8\u09AE\u09A8\u09C0\u09AF\u09BC\u09A4\u09BE \u0985\u09AB\u09BE\u09B0\
  \ \u0995\u09B0\u09C7\u0964."
title: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AC\u09C7\u09B0\
  \ \u0995\u09B0\u09BE"
weight: 6
---

## কিভাবে:
Dart-এ, আপনি বিভিন্ন পদ্ধতি ব্যবহার করে সাবস্ট্রিং এক্সট্রাক্ট করতে পারেন, যেমন `substring()`, `split()`, এবং রেগুলার এক্সপ্রেশন। প্রতিটি পদ্ধতি বিভিন্ন উদ্দেশ্যে সেবা করে এবং স্ট্রিং হ্যান্ডলিং-এ নমনীয়তা অফার করে।

### `substring()` ব্যবহার করে:
`substring()` পদ্ধতিটি সরল। আপনি স্ট্রিংকে স্লাইস করার জন্য শুরু (এবং ঐচ্ছিকভাবে, শেষ) ইনডেক্স নির্দিষ্ট করতে পারেন।

```dart
void main() {
  String example = "Hello, World!";
  String result = example.substring(7, 12);
  print(result); // আউটপুট: World
}
```

### `split()` ব্যবহার করে:
একটি স্ট্রিংকে একটি প্যাটার্ন (যেমন একটি স্পেস বা কমা) অনুযায়ী সাবস্ট্রিংগের একটি তালিকায় ভাগ করুন, এবং তারপর ইনডেক্স অনুযায়ী সাবস্ট্রিং অ্যাক্সেস করুন।

```dart
void main() {
  String example = "Dart is fun";
  List<String> parts = example.split(' ');
  String result = parts[1]; // ইনডেক্স অনুযায়ী অ্যাক্সেস
  print(result); // আউটপুট: is
}
```

### রেগুলার এক্সপ্রেশন ব্যবহার করে:
জটিল প্যাটার্নের জন্য, Dart-এর `RegExp` ক্লাস শক্তিশালী। এটি প্যাটার্ন মিলানো এবং সাবস্ট্রিং এক্সট্রাক্ট করার জন্য ব্যবহার করুন।

```dart
void main() {
  String example = "Email: example@mail.com";
  RegExp regExp = RegExp(r"\b\w+@\w+\.\w+\b");
  String email = regExp.stringMatch(example)!;
  print(email); // আউটপুট: example@mail.com
}
```

### থার্ড-পার্টি লাইব্রেরি:
যদিও Dart-এর স্ট্যান্ডার্ড লাইব্রেরি বেশ সক্ষম, আপনি এমন সিনারিওতে মুখোমুখি হতে পারেন যেখানে একটি থার্ড-পার্টি লাইব্রেরি আপনার কাজ সহজ করে দিতে পারে। স্ট্রিং ম্যানিপুলেশন এবং প্যাটার্ন ম্যাচিংয়ের জন্য জনপ্রিয় পছন্দ এখানে নির্দিষ্টভাবে অধিবাদিত নয় কারণ Dart-এর নিজস্ব সুবিধাগুলি প্রায়ই যথেষ্ট। তবে, আপনার নির্দিষ্ট প্রয়োজনাবলী অনুসারে যে কোন লাইব্রেরি যা আরো ভালভাবে অনুকূল হতে পারে তার জন্য সবসময় [pub.dev](https://pub.dev) পরীক্ষা করুন।

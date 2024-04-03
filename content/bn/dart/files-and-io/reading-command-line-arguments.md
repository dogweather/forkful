---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:09:52.542368-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Dart `List<String> args` \u098F\
  \u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u0995\u09AE\u09BE\u09A8\u09CD\
  \u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\u09B0\u09CD\u0997\u09C1\u09AE\u09C7\u09A8\
  \u09CD\u099F\u0997\u09C1\u09B2\u09BF \u0985\u09CD\u09AF\u09BE\u0995\u09CD\u09B8\u09C7\
  \u09B8 \u0995\u09B0\u09BE\u09B0 \u098F\u0995\u099F\u09BF \u09B8\u09B9\u099C \u09AA\
  \u09A6\u09CD\u09A7\u09A4\u09BF \u09AA\u09CD\u09B0\u09B8\u09CD\u09A4\u09BE\u09AC\
  \ \u0995\u09B0\u09C7, \u09AF\u09BE \u09AE\u09C2\u09B2 \u09AA\u09A6\u09CD\u09A7\u09A4\
  \u09BF\u09A4\u09C7 \u09A5\u09BE\u0995\u09C7\u0964 \u09A8\u09BF\u099A\u09C7 \u0995\
  \u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8\u2026"
lastmod: '2024-03-17T18:47:43.736242-06:00'
model: gpt-4-0125-preview
summary: "Dart `List<String> args` \u098F\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\
  \u09C7 \u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\u09B0\
  \u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F\u0997\u09C1\u09B2\u09BF \u0985\u09CD\
  \u09AF\u09BE\u0995\u09CD\u09B8\u09C7\u09B8 \u0995\u09B0\u09BE\u09B0 \u098F\u0995\
  \u099F\u09BF \u09B8\u09B9\u099C \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF \u09AA\u09CD\
  \u09B0\u09B8\u09CD\u09A4\u09BE\u09AC \u0995\u09B0\u09C7, \u09AF\u09BE \u09AE\u09C2\
  \u09B2 \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF\u09A4\u09C7 \u09A5\u09BE\u0995\u09C7\
  \u0964 \u09A8\u09BF\u099A\u09C7 \u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\
  \u0987\u09A8 \u0986\u09B0\u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F\u0997\u09C1\
  \u09B2\u09BF \u09AA\u09A1\u09BC\u09BE \u098F\u09AC\u0982 \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09BE\u09B0 \u098F\u0995\u099F\u09BF \u09B8\u09BE\
  \u09A7\u09BE\u09B0\u09A3 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\
  \u09AF\u09BC\u09BE \u09B9\u09B2\u0964."
title: "\u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\u09B0\
  \u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F\u0997\u09C1\u09B2\u09BF \u09AA\u09A1\
  \u09BC\u09BE"
weight: 23
---

## কিভাবে:
Dart `List<String> args` এর মাধ্যমে কমান্ড লাইন আর্গুমেন্টগুলি অ্যাক্সেস করার একটি সহজ পদ্ধতি প্রস্তাব করে, যা মূল পদ্ধতিতে থাকে। নিচে কমান্ড লাইন আর্গুমেন্টগুলি পড়া এবং ব্যবহার করার একটি সাধারণ উদাহরণ দেওয়া হল।

```dart
// main.dart
void main(List<String> args) {
  print('Command Line Arguments:');
  for (var i = 0; i < args.length; i++) {
    print('${i + 1}: ${args[i]}');
  }
}
```

এই Dart প্রোগ্রামটি চালানো এবং কমান্ড লাইন আর্গুমেন্ট পাস করার জন্য, Dart CLI এর মতো ব্যবহার করুন:

```shell
dart run main.dart Hello World!
```

প্রত্যাশিত আউটপুট:

```
Command Line Arguments:
1: Hello
2: World!
```

### একটি জনপ্রিয় থার্ড-পার্টি লাইব্রেরি ব্যবহার করা: `args`
যদিও Dart-এর নিজস্ব ক্ষমতাগুলি অনেক অ্যাপ্লিকেশনের জন্য প্রবল, `args` প্যাকেজটি আরও জটিল প্রয়োজনীয়তাগুলোর জন্য কমান্ড লাইন আর্গুমেন্ট সংজ্ঞায়িত এবং পার্স করার একটি উন্নত উপায় প্রদান করে।

প্রথমে, `pubspec.yaml`-এ `args` প্যাকেজটি যোগ করুন:

```yaml
dependencies:
  args: ^2.0.0
```

তারপর, আপনার প্রোগ্রামে এটি ব্যবহার করুন এরকমভাবে:

```dart
// 'args' প্যাকেজ ব্যবহার করা হচ্ছে
import 'package:args/args.dart';

void main(List<String> arguments) {
  final parser = ArgParser()..addOption('name', abbr: 'n');
  final argResults = parser.parse(arguments);

  if (argResults.wasParsed('name')) {
    print('Hello, ${argResults['name']}!');
  } else {
    print('No name provided.');
  }
}
```

নামযুক্ত আর্গুমেন্টের সাথে প্রোগ্রামটি চালান:

```shell
dart run main.dart --name=John
```

প্রত্যাশিত আউটপুট:

```
Hello, John!
```

কমান্ড লাইন আর্গুমেন্ট পার্স করার এই সাধারণ পরিচয়, উভয় নেটিভ এবং `args` লাইব্রেরির সাথে, Dart-এর কনসোল থেকে সরাসরি ইউজার ইনপুট হ্যান্ডেল করার ক্ষমতা দেখায়, যা আরও ইন্টার‌্যাক্টিভ এবং ডা্যানামিক CLI অ্যাপ্লিকেশন তৈরির পথ খোলে।

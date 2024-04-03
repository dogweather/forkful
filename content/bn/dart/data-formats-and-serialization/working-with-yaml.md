---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:36:57.584730-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Dart-\u098F, YAML \u098F\u09B0\
  \ \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE \u09B8\u09BE\u09A7\
  \u09BE\u09B0\u09A3\u09A4 \u098F\u0995\u099F\u09BF \u09A4\u09C3\u09A4\u09C0\u09AF\
  \u09BC \u09AA\u0995\u09CD\u09B7\u09C0\u09AF\u09BC \u09B2\u09BE\u0987\u09AC\u09CD\
  \u09B0\u09C7\u09B0\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09C7 \u09B9\u09DF \u0995\u09BE\u09B0\u09A3 \u098F\u0987 \u09AD\u09BE\u09B7\u09BE\
  \u09DF YAML \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982 \u098F\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u09A8\u09BF\u09B0\u09CD\u09AE\u09BF\u09A4 \u0995\u09CD\u09B7\u09AE\
  \u09A4\u09BE \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09AD\u09C1\u0995\u09CD\u09A4\u2026"
lastmod: '2024-03-17T18:47:43.741482-06:00'
model: gpt-4-0125-preview
summary: "Dart-\u098F, YAML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C\
  \ \u0995\u09B0\u09BE \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\u09A4 \u098F\u0995\u099F\
  \u09BF \u09A4\u09C3\u09A4\u09C0\u09AF\u09BC \u09AA\u0995\u09CD\u09B7\u09C0\u09AF\
  \u09BC \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09B9\u09DF \u0995\u09BE\u09B0\u09A3\
  \ \u098F\u0987 \u09AD\u09BE\u09B7\u09BE\u09DF YAML \u09AA\u09BE\u09B0\u09CD\u09B8\
  \u09BF\u0982 \u098F\u09B0 \u099C\u09A8\u09CD\u09AF \u09A8\u09BF\u09B0\u09CD\u09AE\
  \u09BF\u09A4 \u0995\u09CD\u09B7\u09AE\u09A4\u09BE \u0985\u09A8\u09CD\u09A4\u09B0\
  \u09CD\u09AD\u09C1\u0995\u09CD\u09A4 \u09A8\u09C7\u0987\u0964 \u098F\u0995\u099F\
  \u09BF \u099C\u09A8\u09AA\u09CD\u09B0\u09BF\u09AF\u09BC \u09AC\u09BF\u0995\u09B2\
  \u09CD\u09AA \u09B9\u09B2 `yaml` \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C\u0964\
  \ \u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF, \u0986\
  \u09AA\u09A8\u09BE\u0995\u09C7 \u098F\u0987 \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\
  \u099C\u099F\u09BF \u0986\u09AA\u09A8\u09BE\u09B0 `pubspec.yaml`-\u098F \u09AF\u09CB\
  \u0997 \u0995\u09B0\u09A4\u09C7 \u09B9\u09AC\u09C7."
title: "\u0987\u09DF\u09BE\u09AE\u09C7\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE"
weight: 41
---

## কিভাবে:
Dart-এ, YAML এর সাথে কাজ করা সাধারণত একটি তৃতীয় পক্ষীয় লাইব্রেরি ব্যবহার করে হয় কারণ এই ভাষায় YAML পার্সিং এর জন্য নির্মিত ক্ষমতা অন্তর্ভুক্ত নেই। একটি জনপ্রিয় বিকল্প হল `yaml` প্যাকেজ। শুরু করার জন্য, আপনাকে এই প্যাকেজটি আপনার `pubspec.yaml`-এ যোগ করতে হবে:

```yaml
dependencies:
  yaml: ^3.1.0
```

`pub get` চালানোর মনে রাখুন যাতে প্যাকেজটি আনা হয়।

### YAML পঠন
YAML ফাইল পড়তে, প্রথমে, `yaml` প্যাকেজটি ইম্পোর্ট করুন এবং তারপর `loadYaml` ফাংশনটি ব্যবহার করুন:

```dart
import 'package:yaml/yaml.dart';
import 'dart:io';

void main() {
  final file = File('config.yaml').readAsStringSync();
  final yamlMap = loadYaml(file);

  print(yamlMap['name']); // আউটপুট: John Doe
}

```

ধরুন আপনার `config.yaml` ফাইলটি এমন দেখাচ্ছে:

```yaml
name: John Doe
age: 30
```

### YAML লেখা
`yaml` প্যাকেজ পার্সিং এর জন্য দারুণ, কিন্তু YAML লেখার সাপোর্ট করে না। এর জন্য, আপনাকে হয়তো আপনার ডেটা ম্যানুয়ালি YAML এ রূপান্তর করতে হবে অথবা উপলব্ধ অন্য কোন প্যাকেজ ব্যবহার করতে হবে। অথবা, সোজাসুজি, আপনার ডেটা ট্রান্সফরমেশনগুলি পরিচালনা করে এবং YAML সিনট্যাক্সের সাথে মিল রেখে স্ট্রিং হিসেবে আউটপুট দিন:

```dart
Map<String, dynamic> data = {
  'name': 'Jane Doe',
  'age': 29,
};

String toYamlString(Map<String, dynamic> map) {
  String yaml = '';
  map.forEach((key, value) {
    yaml += '$key: $value\n';
  });
  return yaml;
}

void main() {
  print(toYamlString(data)); // আউটপুট: name: Jane Doe
                             //         age: 29
}
```

এটি একটি মৌলিক পদ্ধতি এবং জটিল ডেটা সংরচনা বা বিশেষ YAML বৈশিষ্ট্যের জন্য উপযুক্ত নাও হতে পারে। জটিল প্রয়োজনগুলির জন্য, আপনাকে হয়তো একটি আরও সম্পূর্ণ Dart প্যাকেজের সন্ধান করতে হবে অথবা অবদান রাখতে হবে।

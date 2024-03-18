---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:36:57.584730-06:00
description: "YAML, \u09AF\u09BE\u09B0 \u09AA\u09C2\u09B0\u09CD\u09A3 \u09B0\u09C2\
  \u09AA YAML Ain't Markup Language, \u09B9\u09B2 \u098F\u0995 \u09A7\u09B0\u09A3\u09C7\
  \u09B0 \u09AE\u09BE\u09A8\u09AC-\u09AA\u09BE\u09A0\u09CD\u09AF\u09AF\u09CB\u0997\
  \u09CD\u09AF \u09A1\u09C7\u099F\u09BE \u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\
  \u09B2\u09BE\u0987\u099C\u09C7\u09B6\u09A8 \u09AB\u09B0\u09CD\u09AE\u09CD\u09AF\u09BE\
  \u099F\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\
  \u09BE \u098F\u099F\u09BF \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\
  \u09A8 \u09AB\u09BE\u0987\u09B2, \u09A1\u09C7\u099F\u09BE \u09AC\u09BF\u09A8\u09BF\
  \u09AE\u09AF\u09BC,\u2026"
lastmod: '2024-03-17T18:47:43.741482-06:00'
model: gpt-4-0125-preview
summary: "YAML, \u09AF\u09BE\u09B0 \u09AA\u09C2\u09B0\u09CD\u09A3 \u09B0\u09C2\u09AA\
  \ YAML Ain't Markup Language, \u09B9\u09B2 \u098F\u0995 \u09A7\u09B0\u09A3\u09C7\
  \u09B0 \u09AE\u09BE\u09A8\u09AC-\u09AA\u09BE\u09A0\u09CD\u09AF\u09AF\u09CB\u0997\
  \u09CD\u09AF \u09A1\u09C7\u099F\u09BE \u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\
  \u09B2\u09BE\u0987\u099C\u09C7\u09B6\u09A8 \u09AB\u09B0\u09CD\u09AE\u09CD\u09AF\u09BE\
  \u099F\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\
  \u09BE \u098F\u099F\u09BF \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\
  \u09A8 \u09AB\u09BE\u0987\u09B2, \u09A1\u09C7\u099F\u09BE \u09AC\u09BF\u09A8\u09BF\
  \u09AE\u09AF\u09BC,\u2026"
title: "\u0987\u09DF\u09BE\u09AE\u09C7\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কী এবং কেন?

YAML, যার পূর্ণ রূপ YAML Ain't Markup Language, হল এক ধরণের মানব-পাঠ্যযোগ্য ডেটা সিরিয়ালাইজেশন ফর্ম্যাট। প্রোগ্রামাররা এটি কনফিগারেশন ফাইল, ডেটা বিনিময়, এবং এমন এপ্লিকেশনগুলিতে ব্যবহার করে থাকেন যেখানে ডেটাকে এমন একটি ফর্ম্যাটে সংরক্ষণ বা প্রেরণ করা প্রয়োজন যা বুঝতে সহজ।

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

---
title:                "ইয়ামেল নিয়ে কাজ করা"
date:                  2024-03-17T18:36:57.584730-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
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

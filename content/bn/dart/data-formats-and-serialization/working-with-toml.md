---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:30:24.580173-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Dart \u09AD\u09BE\u09B7\u09BE\u09AF\
  \u09BC TOML \u098F\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09BF\u09B2\u09CD\u099F\
  -\u0987\u09A8 \u09B8\u09BE\u09AA\u09CB\u09B0\u09CD\u099F \u0985\u09A8\u09CD\u09A4\
  \u09B0\u09CD\u09AD\u09C1\u0995\u09CD\u09A4 \u09A8\u09C7\u0987, \u09A4\u09AC\u09C7\
  \ \u0986\u09AA\u09A8\u09BF `toml` \u09AE\u09A4\u09CB \u09A4\u09C3\u09A4\u09C0\u09AF\
  \u09BC-\u09AA\u0995\u09CD\u09B7\u09C7\u09B0 \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\
  \u099C\u0997\u09C1\u09B2\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09C7 TOML \u09AB\u09BE\u0987\u09B2\u0997\u09C1\u09B2\u09BF \u09A8\u09BF\u09AF\
  \u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09A4\u09C7\u2026"
lastmod: '2024-03-17T18:47:43.744555-06:00'
model: gpt-4-0125-preview
summary: "Dart \u09AD\u09BE\u09B7\u09BE\u09AF\u09BC TOML \u098F\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u09B8\u09BE\u09AA\u09CB\
  \u09B0\u09CD\u099F \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09AD\u09C1\u0995\u09CD\u09A4\
  \ \u09A8\u09C7\u0987, \u09A4\u09AC\u09C7 \u0986\u09AA\u09A8\u09BF `toml` \u09AE\u09A4\
  \u09CB \u09A4\u09C3\u09A4\u09C0\u09AF\u09BC-\u09AA\u0995\u09CD\u09B7\u09C7\u09B0\
  \ \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C\u0997\u09C1\u09B2\u09BF \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 TOML \u09AB\u09BE\u0987\u09B2\u0997\
  \u09C1\u09B2\u09BF \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u09AA\u09CD\u09B0\u09A5\u09AE\
  \u09C7, \u0986\u09AA\u09A8\u09BE\u09B0 `pubspec.yaml`-\u098F `toml` \u09AF\u09CB\
  \u0997 \u0995\u09B0\u09C1\u09A8."
title: "\u099F\u09AE\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\
  \u09B0\u09BE"
weight: 39
---

## কিভাবে:
Dart ভাষায় TOML এর জন্য বিল্ট-ইন সাপোর্ট অন্তর্ভুক্ত নেই, তবে আপনি `toml` মতো তৃতীয়-পক্ষের প্যাকেজগুলি ব্যবহার করে TOML ফাইলগুলি নিয়ে কাজ করতে পারেন। প্রথমে, আপনার `pubspec.yaml`-এ `toml` যোগ করুন:

```yaml
dependencies:
  toml: ^0.10.0
```

### TOML পড়া
TOML ফাইল পড়তে, ধরে নেওয়া যাক আপনার কাছে একটি সাধারণ কনফিগারেশন ফাইল `config.toml` আছে:

```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

Dart এ আপনি এই TOML ফাইলটি নিম্নরূপ পার্স করতে পারেন:

```dart
import 'dart:io';
import 'package:toml/toml.dart';

void main() async {
  var content = await File('config.toml').readAsString();
  var doc = TomlDocument.parse(content);
  var data = doc.toMap();

  print(data['database']); // 'database' সেকশনটি প্রিন্ট করা
}
```

এটি প্রিন্ট করবে:

```dart
{server: 192.168.1.1, ports: [8001, 8001, 8002], connection_max: 5000, enabled: true}
```

### TOML লেখা
TOML কনটেন্ট তৈরির জন্য, `toml` প্যাকেজের দ্বারা প্রদান করা `TomlBuilder` ব্যবহার করুন:

```dart
import 'package:toml/toml.dart';

void main() {
  final builder = TomlBuilder();

  builder.table('database')
    ..set('server', '192.168.1.1')
    ..set('ports', [8001, 8001, 8002])
    ..set('connection_max', 5000)
    ..set('enabled', true);

  var tomlString = builder.build().toString();
  print(tomlString);
}
```

এটি একটি স্ট্রিং প্রতিনিধিত্ব তৈরি করবে এবং প্রিন্ট করবে যা আমাদের `config.toml` ফাইলের মতো দেখতে হবে:

```toml
[database]
server = "192.168.1.1"
ports = [8001, 8001, 8002]
connection_max = 5000
enabled = true
```

এই উদাহরণগুলি দেখায় কিভাবে TOML ফাইল থেকে পড়া এবং লিখা যায়, যা আপনার Dart অ্যাপ্লিকেশনগুলিতে কনফিগারেশন ডেটা নিয়ে কাজ করা সহজ করে তোলে।

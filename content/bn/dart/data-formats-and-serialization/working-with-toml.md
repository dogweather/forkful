---
title:                "টমল নিয়ে কাজ করা"
date:                  2024-03-17T18:30:24.580173-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি ও কেন?

TOML, বা Tom's Obvious, Minimal Language, একটি কনফিগারেশন ফাইল ফরম্যাট যা এর স্পষ্ট সেমান্টিক্সের কারণে পড়া সহজ। প্রোগ্রামাররা এটি সফ্টওয়্যার অ্যাপ্লিকেশনগুলি কনফিগার করার জন্য ব্যবহার করে কারণ এটি পার্স করা সরল এবং এটি ন্যূনতম বিভ্রান্তি বা ত্রুটি সৃষ্টি করে।

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

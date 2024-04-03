---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:39.697717-07:00
description: "TOML, vi\u1EBFt t\u1EAFt c\u1EE7a Tom's Obvious, Minimal Language, l\xE0\
  \ \u0111\u1ECBnh d\u1EA1ng file c\u1EA5u h\xECnh \u0111\u01B0\u1EE3c thi\u1EBFt\
  \ k\u1EBF \u0111\u1EC3 d\u1EC5 \u0111\u1ECDc do ng\u1EEF ngh\u0129a r\xF5 r\xE0\
  ng c\u1EE7a n\xF3. C\xE1c l\u1EADp tr\xECnh vi\xEAn s\u1EED\u2026"
lastmod: '2024-03-13T22:44:36.293847-06:00'
model: gpt-4-0125-preview
summary: "TOML, vi\u1EBFt t\u1EAFt c\u1EE7a Tom's Obvious, Minimal Language, l\xE0\
  \ \u0111\u1ECBnh d\u1EA1ng file c\u1EA5u h\xECnh \u0111\u01B0\u1EE3c thi\u1EBFt\
  \ k\u1EBF \u0111\u1EC3 d\u1EC5 \u0111\u1ECDc do ng\u1EEF ngh\u0129a r\xF5 r\xE0\
  ng c\u1EE7a n\xF3."
title: "L\xE0m vi\u1EC7c v\u1EDBi TOML"
weight: 39
---

## Cái gì & Tại sao?

TOML, viết tắt của Tom's Obvious, Minimal Language, là định dạng file cấu hình được thiết kế để dễ đọc do ngữ nghĩa rõ ràng của nó. Các lập trình viên sử dụng nó để cấu hình các ứng dụng phần mềm vì nó dễ phân tích cú pháp và tạo ra ít nhầm lẫn hoặc lỗi.

## Làm thế nào:

Dart không bao gồm hỗ trợ sẵn cho TOML, nhưng bạn có thể làm việc với các file TOML sử dụng các gói bên thứ ba như `toml`. Đầu tiên, thêm `toml` vào `pubspec.yaml` của bạn:

```yaml
dependencies:
  toml: ^0.10.0
```

### Đọc TOML

Để đọc một file TOML, giả sử bạn có một file cấu hình đơn giản `config.toml`:

```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

Bạn có thể phân tích cú pháp file TOML này trong Dart như sau:

```dart
import 'dart:io';
import 'package:toml/toml.dart';

void main() async {
  var content = await File('config.toml').readAsString();
  var doc = TomlDocument.parse(content);
  var data = doc.toMap();

  print(data['database']); // In ra mục 'database'
}
```

Kết quả in ra sẽ là:

```dart
{server: 192.168.1.1, ports: [8001, 8001, 8002], connection_max: 5000, enabled: true}
```

### Viết TOML

Để tạo nội dung TOML, sử dụng `TomlBuilder` được cung cấp bởi gói `toml`:

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

Điều này sẽ tạo và in ra một chuỗi biểu diễn nội dung TOML, rất giống với file `config.toml` của chúng ta:

```toml
[database]
server = "192.168.1.1"
ports = [8001, 8001, 8002]
connection_max = 5000
enabled = true
```

Những ví dụ này cho thấy làm thế nào để đọc từ và viết tới các file TOML, làm cho việc làm việc với dữ liệu cấu hình trong các ứng dụng Dart của bạn trở nên đơn giản.

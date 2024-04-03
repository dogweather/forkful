---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:39.697717-07:00
description: "L\xE0m th\u1EBF n\xE0o: Dart kh\xF4ng bao g\u1ED3m h\u1ED7 tr\u1EE3\
  \ s\u1EB5n cho TOML, nh\u01B0ng b\u1EA1n c\xF3 th\u1EC3 l\xE0m vi\u1EC7c v\u1EDB\
  i c\xE1c file TOML s\u1EED d\u1EE5ng c\xE1c g\xF3i b\xEAn th\u1EE9 ba nh\u01B0 `toml`.\
  \ \u0110\u1EA7u ti\xEAn, th\xEAm\u2026"
lastmod: '2024-03-13T22:44:36.293847-06:00'
model: gpt-4-0125-preview
summary: "Dart kh\xF4ng bao g\u1ED3m h\u1ED7 tr\u1EE3 s\u1EB5n cho TOML, nh\u01B0\
  ng b\u1EA1n c\xF3 th\u1EC3 l\xE0m vi\u1EC7c v\u1EDBi c\xE1c file TOML s\u1EED d\u1EE5\
  ng c\xE1c g\xF3i b\xEAn th\u1EE9 ba nh\u01B0 `toml`."
title: "L\xE0m vi\u1EC7c v\u1EDBi TOML"
weight: 39
---

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

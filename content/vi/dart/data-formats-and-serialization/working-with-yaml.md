---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:35.547913-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Trong Dart, l\xE0m vi\u1EC7c v\u1EDBi YAML\
  \ th\u01B0\u1EDDng li\xEAn quan \u0111\u1EBFn vi\u1EC7c s\u1EED d\u1EE5ng m\u1ED9\
  t th\u01B0 vi\u1EC7n b\xEAn th\u1EE9 ba v\xEC ng\xF4n ng\u1EEF n\xE0y kh\xF4ng bao\
  \ g\u1ED3m kh\u1EA3 n\u0103ng ph\xE2n t\xEDch\u2026"
lastmod: '2024-03-13T22:44:36.290075-06:00'
model: gpt-4-0125-preview
summary: "Trong Dart, l\xE0m vi\u1EC7c v\u1EDBi YAML th\u01B0\u1EDDng li\xEAn quan\
  \ \u0111\u1EBFn vi\u1EC7c s\u1EED d\u1EE5ng m\u1ED9t th\u01B0 vi\u1EC7n b\xEAn th\u1EE9\
  \ ba v\xEC ng\xF4n ng\u1EEF n\xE0y kh\xF4ng bao g\u1ED3m kh\u1EA3 n\u0103ng ph\xE2\
  n t\xEDch c\xFA ph\xE1p YAML t\xEDch h\u1EE3p s\u1EB5n."
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
weight: 41
---

## Cách thực hiện:
Trong Dart, làm việc với YAML thường liên quan đến việc sử dụng một thư viện bên thứ ba vì ngôn ngữ này không bao gồm khả năng phân tích cú pháp YAML tích hợp sẵn. Một lựa chọn phổ biến là gói `yaml`. Để bắt đầu, bạn cần thêm gói này vào `pubspec.yaml` của mình:

```yaml
dependencies:
  yaml: ^3.1.0
```

Nhớ chạy `pub get` để tải gói.

### Đọc YAML
Để đọc một tệp YAML, đầu tiên, nhập gói `yaml` và sau đó sử dụng hàm `loadYaml`:

```dart
import 'package:yaml/yaml.dart';
import 'dart:io';

void main() {
  final file = File('config.yaml').readAsStringSync();
  final yamlMap = loadYaml(file);

  print(yamlMap['name']); // Đầu ra: John Doe
}

```

Giả sử tệp `config.yaml` của bạn trông như thế này:

```yaml
name: John Doe
age: 30
```

### Viết YAML
Mặc dù gói `yaml` rất tốt cho việc phân tích, nó không hỗ trợ viết YAML. Đối với điều này, bạn có thể cần chuyển đổi dữ liệu của mình thành YAML một cách thủ công hoặc sử dụng một gói khác nếu có. Hoặc, một cách trực tiếp hơn, quản lý các biến đổi dữ liệu của bạn và xuất chúng dưới dạng chuỗi khớp với cú pháp YAML:

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
  print(toYamlString(data)); // Đầu ra: name: Jane Doe
                             //         age: 29
}
```

Đây là một phương pháp cơ bản và có thể không phù hợp với các cấu trúc dữ liệu phức tạp hoặc các tính năng YAML đặc biệt. Đối với những nhu cầu phức tạp, bạn có thể cần tìm hoặc đóng góp vào một gói Dart toàn diện hơn.

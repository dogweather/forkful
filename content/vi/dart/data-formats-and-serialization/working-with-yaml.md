---
title:                "Làm việc với YAML"
date:                  2024-03-08T21:57:35.547913-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

YAML, viết tắt của YAML Ain't Markup Language, là một định dạng tuần tự hóa dữ liệu dễ đọc cho con người. Lập trình viên sử dụng nó cho các tệp cấu hình, trao đổi dữ liệu và trong các ứng dụng cần lưu trữ hoặc truyền dữ liệu trong một định dạng dễ hiểu.

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

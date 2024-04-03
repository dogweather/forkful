---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:03.774161-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Trong Dart, b\u1EA1n c\xF3 th\u1EC3 in\
  \ th\xF4ng b\xE1o g\u1EE1 r\u1ED1i b\u1EB1ng c\xE1ch s\u1EED d\u1EE5ng h\xE0m `print()`.\
  \ D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch xu\u1EA5t th\xF4ng \u0111i\u1EC7p \u0111\
  \u01A1n gi\u1EA3n v\xE0 gi\xE1 tr\u1ECB c\u1EE7a bi\u1EBFn."
lastmod: '2024-03-13T22:44:36.266374-06:00'
model: gpt-4-0125-preview
summary: "Trong Dart, b\u1EA1n c\xF3 th\u1EC3 in th\xF4ng b\xE1o g\u1EE1 r\u1ED1i\
  \ b\u1EB1ng c\xE1ch s\u1EED d\u1EE5ng h\xE0m `print()`."
title: "In ra th\xF4ng tin g\u1EE1 l\u1ED7i"
weight: 33
---

## Cách thực hiện:
Trong Dart, bạn có thể in thông báo gỡ rối bằng cách sử dụng hàm `print()`. Dưới đây là cách xuất thông điệp đơn giản và giá trị của biến:

```dart
void main() {
  String greeting = "Chào, Dart!";
  print(greeting); // In ra: Chào, Dart!

  int number = 42;
  print('Con số là $number.'); // In ra: Con số là 42.
}
```

Đối với dữ liệu có cấu trúc, như danh sách hoặc đối tượng, phương thức `toString()` của Dart có thể không cung cấp đủ chi tiết. Trong những trường hợp đó, bạn có thể sử dụng hàm `jsonEncode` từ thư viện `dart:convert` của Dart để chuyển đổi dữ liệu thành chuỗi JSON cho đầu ra dễ đọc hơn:

```dart
import 'dart:convert';

void main() {
  var user = {
    'name': 'John Doe',
    'age': 30,
    'emails': ['john.doe@example.com', 'john@example.com'],
  };

  print(jsonEncode(user));
  // In ra: {"name":"John Doe","age":30,"emails":["john.doe@example.com","john@example.com"]}
}
```

Khi cần các khả năng gỡ lỗi tinh vi hơn, chẳng hạn như ghi nhật ký với các mức độ quan trọng khác nhau (thông tin, cảnh báo, lỗi), bạn có thể sử dụng các thư viện bên thứ ba như `logger`. Dưới đây là cách sử dụng nó:

1. Thêm `logger` vào `pubspec.yaml` của bạn:

```yaml
dependencies:
  logger: ^1.0.0
```

2. Sử dụng `logger` trong mã Dart của bạn:

```dart
import 'package:logger/logger.dart';

var logger = Logger();

void main() {
  logger.d("Đây là một thông điệp gỡ lỗi");
  logger.w("Đây là một thông điệp cảnh báo");
  logger.e("Đây là một thông điệp lỗi");
}
```

Đầu ra sẽ thông tin hơn, hiển thị mức độ của thông điệp và thông điệp đó, giúp dễ dàng phân biệt giữa các loại thông điệp nhật ký khác nhau.

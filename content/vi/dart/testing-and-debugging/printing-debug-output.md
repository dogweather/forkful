---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:03.774161-07:00
description: "Vi\u1EC7c in th\xF4ng b\xE1o g\u1EE1 r\u1ED1i trong Dart li\xEAn quan\
  \ \u0111\u1EBFn vi\u1EC7c hi\u1EC3n th\u1ECB th\xF4ng tin l\xEAn b\u1EA3ng \u0111\
  i\u1EC1u khi\u1EC3n trong qu\xE1 tr\xECnh th\u1EF1c thi, cho ph\xE9p c\xE1c nh\xE0\
  \ ph\xE1t tri\u1EC3n theo\u2026"
lastmod: '2024-03-13T22:44:36.266374-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c in th\xF4ng b\xE1o g\u1EE1 r\u1ED1i trong Dart li\xEAn quan \u0111\
  \u1EBFn vi\u1EC7c hi\u1EC3n th\u1ECB th\xF4ng tin l\xEAn b\u1EA3ng \u0111i\u1EC1\
  u khi\u1EC3n trong qu\xE1 tr\xECnh th\u1EF1c thi, cho ph\xE9p c\xE1c nh\xE0 ph\xE1\
  t tri\u1EC3n theo d\xF5i lu\u1ED3ng th\u1EF1c thi, \u0111i\u1EC1u tra tr\u1EA1ng\
  \ th\xE1i c\u1EE7a c\xE1c bi\u1EBFn, ho\u1EB7c x\xE1c \u0111\u1ECBnh ngu\u1ED3n\
  \ g\u1ED1c c\u1EE7a l\u1ED7i."
title: "In ra th\xF4ng tin g\u1EE1 l\u1ED7i"
weight: 33
---

## Cái gì & Tại sao?

Việc in thông báo gỡ rối trong Dart liên quan đến việc hiển thị thông tin lên bảng điều khiển trong quá trình thực thi, cho phép các nhà phát triển theo dõi luồng thực thi, điều tra trạng thái của các biến, hoặc xác định nguồn gốc của lỗi. Các lập trình viên thường sử dụng nó cho việc khắc phục sự cố và chứng minh rằng mã của họ hoạt động như mong đợi, giúp quá trình phát triển suôn sẻ và hiệu quả hơn.

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

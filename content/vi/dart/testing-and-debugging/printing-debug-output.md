---
title:                "In ra thông tin gỡ lỗi"
date:                  2024-03-08T21:56:03.774161-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

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

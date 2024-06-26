---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:09.957909-07:00
description: "L\xE0m th\u1EBF n\xE0o: Dart cung c\u1EA5p c\xE1c ph\u01B0\u01A1ng th\u1EE9\
  c \u0111\u01A1n gi\u1EA3n, d\u1EC5 hi\u1EC3u cho vi\u1EC7c thao t\xE1c chu\u1ED7\
  i. \u0110\u1EC3 vi\u1EBFt hoa m\u1ED9t t\u1EEB ho\u1EB7c m\u1ED9t c\xE2u, b\u1EA1\
  n th\u01B0\u1EDDng l\u1EA5y k\xFD t\u1EF1 \u0111\u1EA7u ti\xEAn,\u2026"
lastmod: '2024-03-13T22:44:36.240320-06:00'
model: gpt-4-0125-preview
summary: "Dart cung c\u1EA5p c\xE1c ph\u01B0\u01A1ng th\u1EE9c \u0111\u01A1n gi\u1EA3\
  n, d\u1EC5 hi\u1EC3u cho vi\u1EC7c thao t\xE1c chu\u1ED7i."
title: "Vi\u1EBFt hoa m\u1ED9t chu\u1ED7i k\xFD t\u1EF1"
weight: 2
---

## Làm thế nào:


### Sử dụng Các Phương Thức Của Dart
Dart cung cấp các phương thức đơn giản, dễ hiểu cho việc thao tác chuỗi. Để viết hoa một từ hoặc một câu, bạn thường lấy ký tự đầu tiên, chuyển nó thành chữ in hoa, và sau đó nối nó với phần còn lại của chuỗi. Dưới đây là cách bạn có thể thực hiện:

```dart
String capitalize(String text) {
  if (text.isEmpty) return text;
  return text[0].toUpperCase() + text.substring(1).toLowerCase();
}

void main() {
  var example = "hello world";
  print(capitalize(example)); // Kết quả: Hello world
}
```

### Viết hoa mỗi từ
Để viết hoa chữ cái đầu tiên của mỗi từ trong một chuỗi, bạn có thể tách chuỗi thành các từ, viết hoa mỗi từ, và sau đó nối chúng lại với nhau:

```dart
String capitalizeWords(String text) {
  return text.split(' ').map(capitalize).join(' ');
}

void main() {
  var example = "hello dart enthusiasts";
  print(capitalizeWords(example)); // Kết quả: Hello Dart Enthusiasts
}
```

### Sử dụng Thư viện Bên thứ ba
Mặc dù thư viện tiêu chuẩn của Dart đáp ứng nhu cầu cơ bản, một số nhiệm vụ có thể được thực hiện một cách tiện lợi hơn bằng cách sử dụng các gói bên thứ ba. Một lựa chọn phổ biến cho các khả năng thao tác chuỗi mở rộng, bao gồm việc viết hoa, là gói [`recase`](https://pub.dev/packages/recase). Sau khi thêm nó vào `pubspec.yaml` của dự án, bạn có thể dễ dàng viết hoa các chuỗi cùng các chức năng khác:

```dart
import 'package:recase/recase.dart';

void main() {
  var example = "hello world";
  var rc = ReCase(example);

  print(rc.titleCase); // Kết quả: Hello World
}
```

Sử dụng `recase`, bạn có thể viết hoa từng từ, toàn bộ câu, hoặc thậm chí tuân theo các quy ước chữ viết khác mà không cần xử lý thủ công các biến đổi chuỗi.

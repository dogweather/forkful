---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:21.040452-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Dart, b\u1EA1n c\xF3 th\u1EC3 chuy\u1EC3\
  n \u0111\u1ED5i m\u1ED9t chu\u1ED7i sang d\u1EA1ng ch\u1EEF th\u01B0\u1EDDng b\u1EB1\
  ng c\xE1ch s\u1EED d\u1EE5ng ph\u01B0\u01A1ng th\u1EE9c `toLowerCase()` \u0111\u01B0\
  \u1EE3c cung c\u1EA5p b\u1EDFi l\u1EDBp\u2026"
lastmod: '2024-03-13T22:44:36.245065-06:00'
model: gpt-4-0125-preview
summary: "Trong Dart, b\u1EA1n c\xF3 th\u1EC3 chuy\u1EC3n \u0111\u1ED5i m\u1ED9t chu\u1ED7\
  i sang d\u1EA1ng ch\u1EEF th\u01B0\u1EDDng b\u1EB1ng c\xE1ch s\u1EED d\u1EE5ng ph\u01B0\
  \u01A1ng th\u1EE9c `toLowerCase()` \u0111\u01B0\u1EE3c cung c\u1EA5p b\u1EDFi l\u1EDB\
  p `String`."
title: "Chuy\u1EC3n \u0111\u1ED5i chu\u1ED7i th\xE0nh ch\u1EEF th\u01B0\u1EDDng"
weight: 4
---

## Làm thế nào:
Trong Dart, bạn có thể chuyển đổi một chuỗi sang dạng chữ thường bằng cách sử dụng phương thức `toLowerCase()` được cung cấp bởi lớp `String`. Phương thức này trả về một chuỗi mới với tất cả các kí tự in hoa được chuyển đổi thành chữ thường. Hãy xem cách thức này hoạt động như thế nào qua một ví dụ đơn giản:

```dart
void main() {
  String chuoiGoc = "Hello, World!";
  String chuoiChuThuong = chuoiGoc.toLowerCase();

  print(chuoiChuThuong);  // Đầu ra: hello, world!
}
```

Dart không yêu cầu thư viện bên ngoài cho các nhiệm vụ xử lý chuỗi cơ bản, bao gồm cả chuyển đổi sang chữ thường, vì lớp `String` của thư viện tiêu chuẩn khá toàn diện. Tuy nhiên, đối với các thao tác phức tạp hơn liên quan đến các quy tắc cụ thể của địa phương, bạn có thể xem xét gói `intl`, cung cấp các tiện ích quốc tế hóa và địa phương hóa, bao gồm chuyển đổi chữ hoa chữ thường dựa trên địa điểm:

Để sử dụng `intl`, thêm nó vào file `pubspec.yaml` của bạn:

```yaml
dependencies:
  intl: ^0.17.0
```

Sau đó, bạn có thể sử dụng phương thức `toLocaleLowerCase()` để chuyển đổi một chuỗi sang dạng chữ thường dựa trên các địa điểm cụ thể:

```dart
import 'package:intl/intl.dart';

void main() {
  String chuoiGoc = "İstanbul";
  
  // Địa điểm tiếng Thổ Nhĩ Kỳ
  print(Intl.withLocale('tr', () => chuoiGoc.toLowerCase())); // Đầu ra: istanbul
  
  // Địa điểm mặc định (en)
  print(chuoiGoc.toLowerCase()); // Đầu ra: i̇stanbul
}
```

Trong ví dụ này, chú ý cách địa điểm tiếng Thổ Nhĩ Kỳ xử lý đúng cách chữ 'i' không dấu, cho thấy tầm quan trọng của các biến đổi nhận thức về địa phương trong các ứng dụng quốc tế hóa.

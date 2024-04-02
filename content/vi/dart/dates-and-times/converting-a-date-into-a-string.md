---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:13.324537-07:00
description: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7i trong\
  \ Dart l\xE0 nhi\u1EC7m v\u1EE5 ph\u1ED5 bi\u1EBFn khi b\u1EA1n c\u1EA7n hi\u1EC3\
  n th\u1ECB th\xF4ng tin ng\xE0y v\xE0 gi\u1EDD d\u01B0\u1EDBi d\u1EA1ng c\xF3 th\u1EC3\
  \ \u0111\u1ECDc \u0111\u01B0\u1EE3c b\u1EDFi con ng\u01B0\u1EDDi, ho\u1EB7c\u2026"
lastmod: '2024-03-13T22:44:36.278281-06:00'
model: gpt-4-0125-preview
summary: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7i trong Dart\
  \ l\xE0 nhi\u1EC7m v\u1EE5 ph\u1ED5 bi\u1EBFn khi b\u1EA1n c\u1EA7n hi\u1EC3n th\u1ECB\
  \ th\xF4ng tin ng\xE0y v\xE0 gi\u1EDD d\u01B0\u1EDBi d\u1EA1ng c\xF3 th\u1EC3 \u0111\
  \u1ECDc \u0111\u01B0\u1EE3c b\u1EDFi con ng\u01B0\u1EDDi, ho\u1EB7c\u2026"
title: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7i"
weight: 28
---

## Lý do và Mục đích?

Chuyển đổi một ngày thành chuỗi trong Dart là nhiệm vụ phổ biến khi bạn cần hiển thị thông tin ngày và giờ dưới dạng có thể đọc được bởi con người, hoặc khi bạn dự định tuần tự hóa (serialize) dữ liệu để lưu trữ hoặc truyền dẫn. Quá trình này cho phép dễ dàng biểu diễn và thao tác với giá trị ngày-giờ trong một định dạng vừa dễ hiểu vừa có thể tùy chỉnh tùy thuộc vào trường hợp sử dụng.

## Cách thực hiện:

Dart cung cấp lớp `DateTime` để xử lý ngày và giờ, và gói `intl` để định dạng. Đầu tiên, hãy đảm bảo bạn có gói `intl` bằng cách thêm `intl: ^0.17.0` (hoặc phiên bản mới nhất) vào tệp `pubspec.yaml` của bạn.

### Sử dụng Thư Viện Cơ Bản của Dart

```dart
DateTime now = DateTime.now();
String formattedDate = "${now.year}-${now.month}-${now.day}";
print(formattedDate); // Đầu ra: 2023-4-12 (ví dụ, điều này tùy thuộc vào ngày hiện tại)
```

Ví dụ này trực tiếp tạo một chuỗi từ các thuộc tính của `DateTime`.

### Sử dụng gói `intl`

Đầu tiên, nhập gói:

```dart
import 'package:intl/intl.dart';
```

Sau đó, định dạng ngày:

```dart
DateTime now = DateTime.now();
String formattedDate = DateFormat('yyyy-MM-dd').format(now);
print(formattedDate); // Đầu ra: 2023-04-12
```

Gói `intl` cho phép dễ dàng định dạng một cách phức tạp hơn, bao gồm cả định dạng dựa theo ngôn ngữ:

```dart
String formattedDateLocale = DateFormat.yMMMMd('en_US').format(now);
print(formattedDateLocale); // Đầu ra: April 12, 2023
```

Những ví dụ này hiển thị những cách đơn giản nhưng mạnh mẽ để chuyển đổi và định dạng ngày thành chuỗi trong Dart, hoặc sử dụng chức năng cốt lõi của Dart hoặc tận dụng gói `intl` cho nhiều tùy chọn định dạng nâng cao hơn.

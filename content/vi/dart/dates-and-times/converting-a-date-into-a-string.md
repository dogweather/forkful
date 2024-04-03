---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:13.324537-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Dart cung c\u1EA5p l\u1EDBp `DateTime`\
  \ \u0111\u1EC3 x\u1EED l\xFD ng\xE0y v\xE0 gi\u1EDD, v\xE0 g\xF3i `intl` \u0111\u1EC3\
  \ \u0111\u1ECBnh d\u1EA1ng. \u0110\u1EA7u ti\xEAn, h\xE3y \u0111\u1EA3m b\u1EA3\
  o b\u1EA1n c\xF3 g\xF3i `intl` b\u1EB1ng c\xE1ch th\xEAm\u2026"
lastmod: '2024-03-13T22:44:36.278281-06:00'
model: gpt-4-0125-preview
summary: "Dart cung c\u1EA5p l\u1EDBp `DateTime` \u0111\u1EC3 x\u1EED l\xFD ng\xE0\
  y v\xE0 gi\u1EDD, v\xE0 g\xF3i `intl` \u0111\u1EC3 \u0111\u1ECBnh d\u1EA1ng."
title: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7i"
weight: 28
---

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

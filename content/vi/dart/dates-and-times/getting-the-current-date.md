---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:01.561400-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Th\u01B0 vi\u1EC7n c\u1ED1t l\xF5i c\u1EE7\
  a Dart cung c\u1EA5p quy\u1EC1n truy c\u1EADp tr\u1EF1c ti\u1EBFp v\xE0o ng\xE0\
  y v\xE0 gi\u1EDD hi\u1EC7n t\u1EA1i th\xF4ng qua l\u1EDBp `DateTime`. D\u01B0\u1EDB\
  i \u0111\xE2y l\xE0 v\xED d\u1EE5 c\u01A1 b\u1EA3n \u0111\u1EC3\u2026"
lastmod: '2024-03-13T22:44:36.276937-06:00'
model: gpt-4-0125-preview
summary: "Th\u01B0 vi\u1EC7n c\u1ED1t l\xF5i c\u1EE7a Dart cung c\u1EA5p quy\u1EC1\
  n truy c\u1EADp tr\u1EF1c ti\u1EBFp v\xE0o ng\xE0y v\xE0 gi\u1EDD hi\u1EC7n t\u1EA1\
  i th\xF4ng qua l\u1EDBp `DateTime`."
title: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i"
weight: 29
---

## Cách thực hiện:
Thư viện cốt lõi của Dart cung cấp quyền truy cập trực tiếp vào ngày và giờ hiện tại thông qua lớp `DateTime`. Dưới đây là ví dụ cơ bản để lấy ngày hiện tại:

```dart
void main() {
  DateTime now = DateTime.now();
  print(now); // Ví dụ đầu ra: 2023-04-12 10:00:00.000
}
```

Nếu bạn chỉ cần phần ngày (năm, tháng, ngày), bạn có thể định dạng đối tượng `DateTime`:

```dart
void main() {
  DateTime now = DateTime.now();
  String formattedDate = "${now.year}-${now.month}-${now.day}";
  print(formattedDate); // Ví dụ đầu ra: 2023-04-12
}
```

Dart không bao gồm thư viện tích hợp sẵn cho việc định dạng ngày phức tạp, nhưng bạn có thể sử dụng gói `intl` cho mục đích này. Trước tiên, thêm gói vào `pubspec.yaml` của bạn:

```yaml
dependencies:
  intl: ^0.17.0
```

Sau đó, bạn có thể định dạng ngày một cách dễ dàng:

```dart
import 'package:intl/intl.dart';

void main() {
  DateTime now = DateTime.now();
  String formattedDate = DateFormat('yyyy-MM-dd').format(now);
  print(formattedDate); // Ví dụ đầu ra: 2023-04-12
}
```

Để tìm hiểu thêm về các tùy chọn định dạng nâng cao, hãy khám phá lớp `DateFormat` được cung cấp bởi gói `intl`, hỗ trợ một loạt các mẫu và địa phương hóa.

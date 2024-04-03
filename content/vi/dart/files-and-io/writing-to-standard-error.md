---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:27.720708-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Dart, vi\u1EBFt v\xE0o stderr r\u1EA5\
  t \u0111\u01A1n gi\u1EA3n s\u1EED d\u1EE5ng \u0111\u1ED1i t\u01B0\u1EE3ng `stderr`\
  \ c\xF3 s\u1EB5n trong `dart:io`. D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t v\xED\
  \ d\u1EE5 c\u01A1 b\u1EA3n."
lastmod: '2024-03-13T22:44:36.284868-06:00'
model: gpt-4-0125-preview
summary: "Trong Dart, vi\u1EBFt v\xE0o stderr r\u1EA5t \u0111\u01A1n gi\u1EA3n s\u1EED\
  \ d\u1EE5ng \u0111\u1ED1i t\u01B0\u1EE3ng `stderr` c\xF3 s\u1EB5n trong `dart:io`."
title: "Ghi v\xE0o l\u1ED7i chu\u1EA9n"
weight: 25
---

## Làm thế nào:
Trong Dart, viết vào stderr rất đơn giản sử dụng đối tượng `stderr` có sẵn trong `dart:io`. Dưới đây là một ví dụ cơ bản:

```dart
import 'dart:io';

void main() {
  stderr.writeln('Đây là một thông điệp lỗi.');
}
```

Đầu ra khi chạy:
```
Đây là một thông điệp lỗi.
```
Thông điệp này được gửi đến luồng stderr, thường được hiển thị trong console hoặc terminal.

Để chứng minh sự phức tạp hơn, như việc ghi nhật ký một ngoại lệ, bộ tính năng phong phú của Dart cho phép xử lý lỗi một cách ngắn gọn và hiệu quả:

```dart
import 'dart:io';

void riskyOperation() {
  try {
    // Giả lập một hoạt động có thể gây ra lỗi
    throw Exception('Có điều gì đó không ổn!');
  } catch (e) {
    stderr.writeln('Lỗi: $e');
  }
}

void main() {
  riskyOperation();
}
```

Đầu ra khi chạy:
```
Lỗi: Exception: Có điều gì đó không ổn!
```

Mẫu này đặc biệt hữu ích cho các ứng dụng cần phân biệt nhật ký bình thường khỏi nhật ký lỗi, làm cho việc giám sát và gỡ lỗi ứng dụng dễ dàng hơn.

Mặc dù thư viện chuẩn của Dart khá toàn diện, nhiều chương trình không yêu cầu thư viện bên thứ ba cho việc viết vào stderr. Tuy nhiên, nếu ứng dụng của bạn cần khả năng ghi nhật ký tinh vi hơn (ví dụ, vào tệp, qua mạng, định dạng), gói `logging` là một lựa chọn phổ biến. Dưới đây là cái nhìn nhanh vào việc sử dụng `logging` cho lỗi:

```dart
import 'dart:io';
import 'package:logging/logging.dart';

final logger = Logger('MyAppLogger');

void setupLogging() {
  logger.onRecord.listen((record) {
    if (record.level >= Level.SEVERE) {
      stderr.writeln('${record.level.name}: ${record.time}: ${record.message}');
    }
  });
}

void main() {
  setupLogging();
  logger.severe('Lỗi Nghiêm Trọng: Đã xảy ra điều cực kỳ tồi tệ.');
}
```

Đầu ra khi chạy:
```
SEVERE: 2023-04-01 00:00:00.000: Lỗi Nghiêm Trọng: Đã xảy ra điều cực kỳ tồi tệ.
```

Phương pháp này cung cấp một mức độ tùy chỉnh và kiểm soát cao hơn về những gì được ghi như một lỗi và cách nó được định dạng, có thể rất hữu ích trong các ứng dụng lớn, phức tạp hơn.

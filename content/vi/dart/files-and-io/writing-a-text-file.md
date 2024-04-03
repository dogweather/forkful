---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:25.903971-07:00
description: "L\xE0m th\u1EBF n\xE0o: Th\u01B0 vi\u1EC7n c\u1ED1t l\xF5i c\u1EE7a\
  \ Dart cung c\u1EA5p g\xF3i `dart:io` cho vi\u1EC7c x\u1EED l\xFD t\u1EC7p, cho\
  \ ph\xE9p b\u1EA1n vi\u1EBFt t\u1EC7p v\u0103n b\u1EA3n m\xE0 kh\xF4ng c\u1EA7n\
  \ c\xE1c th\u01B0 vi\u1EC7n b\xEAn th\u1EE9 ba. D\u01B0\u1EDBi\u2026"
lastmod: '2024-03-13T22:44:36.287499-06:00'
model: gpt-4-0125-preview
summary: "Th\u01B0 vi\u1EC7n c\u1ED1t l\xF5i c\u1EE7a Dart cung c\u1EA5p g\xF3i `dart:io`\
  \ cho vi\u1EC7c x\u1EED l\xFD t\u1EC7p, cho ph\xE9p b\u1EA1n vi\u1EBFt t\u1EC7p\
  \ v\u0103n b\u1EA3n m\xE0 kh\xF4ng c\u1EA7n c\xE1c th\u01B0 vi\u1EC7n b\xEAn th\u1EE9\
  \ ba."
title: "Vi\u1EBFt t\u1EC7p v\u0103n b\u1EA3n"
weight: 24
---

## Làm thế nào:
Thư viện cốt lõi của Dart cung cấp gói `dart:io` cho việc xử lý tệp, cho phép bạn viết tệp văn bản mà không cần các thư viện bên thứ ba. Dưới đây là một ví dụ đơn giản về cách viết tệp văn bản:

```dart
import 'dart:io';

void main() async {
  // Tạo một tệp mới có tên 'example.txt' trong thư mục hiện tại.
  var file = File('example.txt');
  
  // Viết một chuỗi vào tệp.
  await file.writeAsString('Xin chào, Dart!');
  
  // Xác minh nội dung.
  print(await file.readAsString()); // Đầu ra: Xin chào, Dart!
}
```

Khi xử lý các tệp lớn hơn hoặc dòng dữ liệu, bạn có thể ưa thích việc sử dụng `openWrite` vốn trả về một `IOSink` và cho phép bạn viết dữ liệu theo từng phần:

```dart
import 'dart:io';

void main() async {
  var file = File('large_file.txt');
  var sink = file.openWrite();

  // Viết nhiều dòng vào tệp.
  sink
    ..writeln('Dòng 1: Con cáo nâu nhanh nhẹn nhảy qua chú chó lười.')
    ..writeln('Dòng 2: Dart thật tuyệt vời!')
    ..close();

  // Đợi đến khi sink đóng lại để đảm bảo tất cả dữ liệu được viết vào tệp.
  await sink.done;

  // Đọc và in nội dung tệp để xác minh
  print(await file.readAsString());
}
```

Đối với các thao tác tệp nâng cao hơn, bao gồm việc thêm vào tệp hoặc viết byte, bạn có thể tìm hiểu sâu hơn về các phương pháp của lớp `File` được cung cấp bởi `dart:io`. Ngoài ra, khi làm việc trên các dự án quy mô lớn hoặc phức tạp hơn, việc cân nhắc các gói như `path` để xử lý đường dẫn tệp hoặc `shelf` cho các chức năng máy chủ web có thể có lợi, mặc dù việc viết tệp trực tiếp thường dựa vào các thư viện Dart tích hợp sẵn.

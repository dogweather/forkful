---
title:                "Viết tệp văn bản"
date:                  2024-03-08T21:58:25.903971-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?
Việc viết một tệp văn bản trong Dart bao gồm việc tạo hoặc sửa đổi các tệp trên đĩa để lưu trữ dữ liệu dưới dạng có thể đọc được. Lập trình viên thực hiện điều này để lưu trữ dữ liệu ứng dụng, cấu hình, nhật ký hoặc bất kỳ thông tin nào cần được bảo tồn giữa các lần chạy ứng dụng hoặc chia sẻ dữ liệu với các ứng dụng hoặc người dùng khác.

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

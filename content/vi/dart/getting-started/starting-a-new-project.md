---
title:                "Bắt đầu một dự án mới"
date:                  2024-03-08T21:57:05.790403-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Khởi đầu một dự án mới với Dart bao gồm việc thiết lập một môi trường thuận lợi cho việc phát triển, kiểm thử và triển khai hiệu quả. Các lập trình viên bắt đầu các dự án Dart mới để tận dụng hiệu suất tối ưu và hệ sinh thái mạnh mẽ của Dart, đặc biệt là cho việc phát triển ứng dụng web và di động với các framework như Flutter.

## Cách thức:

1. **Cài đặt Dart**:
   Đảm bảo Dart đã được cài đặt trên hệ thống của bạn. Nếu không, bạn có thể tải về từ [https://dart.dev/get-dart](https://dart.dev/get-dart). Xác minh việc cài đặt với:

   ```shell
   dart --version
   ```

2. **Tạo một Dự án Dart Mới**:
   Sử dụng Dart CLI để tạo một dự án mới:

   ```shell
   dart create hello_dart
   ```

   Lệnh này tạo một thư mục mới `hello_dart` với một ứng dụng web hoặc ứng dụng console đơn giản tùy thuộc vào lựa chọn của bạn.

3. **Xem Xét Cấu Trúc Dự án**:
   
   Di chuyển đến thư mục dự án của bạn:

   ```shell
   cd hello_dart
   ```

   Một dự án Dart điển hình bao gồm các tệp và thư mục chính sau:

   - `pubspec.yaml`: Tệp cấu hình bao gồm các phụ thuộc và ràng buộc SDK của dự án bạn.
   - `lib/`: Thư mục chứa phần lớn mã Dart.
   - `test/`: Thư mục cho các bài kiểm tra dự án.

4. **Thêm Phụ Thuộc**:
   Chỉnh sửa `pubspec.yaml` để thêm phụ thuộc. Đối với các dự án web, hãy xem xét thêm `http`, một gói phổ biến để thực hiện các yêu cầu HTTP:

   ```yaml
   dependencies:
     flutter:
       sdk: flutter
     http: ^0.13.3
   ```

   Sau khi chỉnh sửa, lấy các phụ thuộc:

   ```shell
   dart pub get
   ```

5. **Viết Mã Dart Đầu Tiên của Bạn**:
   
   Trong thư mục `lib/`, tạo một tệp Dart mới, `main.dart`, và thêm mã Dart đơn giản:

   ```dart
   // Nhập thư viện cốt lõi Dart
   import 'dart:core';

   void main() {
     print('Hello, Dart!');
   }
   ```

6. **Chạy Ứng Dụng Dart của Bạn**:

   Thực thi chương trình Dart của bạn với:

   ```shell
   dart run
   ```

   Đầu ra sẽ là:

   ```
   Hello, Dart!
   ```

Bằng cách theo dõi các bước này, bạn đã thành công trong việc khởi đầu một dự án Dart mới, từ việc cài đặt đến chạy đoạn mã Dart đầu tiên của bạn. Kiến thức cơ bản này đặt nền móng cho việc đào sâu hơn vào hệ sinh thái phong phú của Dart và khả năng của nó trong việc xây dựng các ứng dụng có thể mở rộng.

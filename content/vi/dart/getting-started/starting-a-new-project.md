---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:05.790403-07:00
description: "Kh\u1EDFi \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi v\u1EDBi Dart\
  \ bao g\u1ED3m vi\u1EC7c thi\u1EBFt l\u1EADp m\u1ED9t m\xF4i tr\u01B0\u1EDDng thu\u1EAD\
  n l\u1EE3i cho vi\u1EC7c ph\xE1t tri\u1EC3n, ki\u1EC3m th\u1EED v\xE0 tri\u1EC3\
  n khai hi\u1EC7u qu\u1EA3. C\xE1c l\u1EADp tr\xECnh vi\xEAn\u2026"
lastmod: '2024-03-13T22:44:36.263791-06:00'
model: gpt-4-0125-preview
summary: "Kh\u1EDFi \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi v\u1EDBi Dart bao\
  \ g\u1ED3m vi\u1EC7c thi\u1EBFt l\u1EADp m\u1ED9t m\xF4i tr\u01B0\u1EDDng thu\u1EAD\
  n l\u1EE3i cho vi\u1EC7c ph\xE1t tri\u1EC3n, ki\u1EC3m th\u1EED v\xE0 tri\u1EC3\
  n khai hi\u1EC7u qu\u1EA3."
title: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi"
weight: 1
---

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

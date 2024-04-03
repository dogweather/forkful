---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:02.242758-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Dart h\u1ED7 tr\u1EE3 hai lo\u1EA1i l\u1ED7\
  i: l\u1ED7i *th\u1EDDi gian bi\xEAn d\u1ECBch* v\xE0 l\u1ED7i *th\u1EDDi gian ch\u1EA1\
  y*. L\u1ED7i th\u1EDDi gian bi\xEAn d\u1ECBch \u0111\u01B0\u1EE3c Dart analyzer\
  \ ph\xE1t hi\u1EC7n tr\u01B0\u1EDBc\u2026"
lastmod: '2024-03-13T22:44:36.272993-06:00'
model: gpt-4-0125-preview
summary: "Dart h\u1ED7 tr\u1EE3 hai lo\u1EA1i l\u1ED7i."
title: "X\u1EED l\xFD l\u1ED7i"
weight: 16
---

## Cách thực hiện:
Dart hỗ trợ hai loại lỗi: lỗi *thời gian biên dịch* và lỗi *thời gian chạy*. Lỗi thời gian biên dịch được Dart analyzer phát hiện trước khi mã chạy, trong khi lỗi thời gian chạy, hay ngoại lệ, xảy ra trong khi thực thi. Dưới đây là cách bạn xử lý ngoại lệ trong Dart:

### Try-Catch
Sử dụng `try-catch` để bắt ngoại lệ và ngăn chúng làm sập ứng dụng của bạn:

```dart
try {
  var result = 100 ~/ 0; // Cố gắng chia cho không, phát ra ngoại lệ
} catch (e) {
  print('Bắt được một ngoại lệ: $e'); // Xử lý ngoại lệ
}
```
Kết quả mẫu: `Bắt được một ngoại lệ: IntegerDivisionByZeroException`

### Ngoại lệ Cụ thể
Để xử lý các ngoại lệ cụ thể, nhắc đến ngoại lệ sau `catch`:

```dart
try {
  var result = 100 ~/ 0;
} on IntegerDivisionByZeroException {
  print('Không thể chia cho không.'); // Xử lý cụ thể các ngoại lệ chia cho không
}
```
Kết quả mẫu: `Không thể chia cho không.`

### Dấu Vết Ngăn Xếp
Để lấy dấu vết ngăn xếp cho việc gỡ lỗi, sử dụng tham số thứ hai trong khối catch:

```dart
try {
  var result = 100 ~/ 0;
} catch (e, s) {
  print('Ngoại lệ: $e');
  print('Dấu vết ngăn xếp: $s'); // In dấu vết ngăn xếp cho việc gỡ lỗi
}
```

### Finally
Sử dụng `finally` để thực thi mã sau try/catch, bất kể liệu có ngoại lệ nào được ném ra hay không:

```dart
try {
  var result = 100 ~/ 0;
} catch (e) {
  print('Bắt được một ngoại lệ: $e');
} finally {
  print('Luôn được thực thi.'); // Mã dọn dẹp hoặc bước cuối cùng
}
```
Kết quả mẫu:
```
Bắt được một ngoại lệ: IntegerDivisionByZeroException
Luôn được thực thi.
```

### Thư Viện Bên Thứ Ba
Mặc dù thư viện cốt lõi của Dart rất mạnh mẽ cho việc xử lý lỗi, bạn cũng có thể sử dụng các gói bên thứ ba như `dartz` cho lập trình hàm mà giới thiệu các khái niệm như `Either` và `Option` có thể được sử dụng cho việc xử lý lỗi. Dưới đây là một ví dụ sử dụng `dartz` để xử lý lỗi:

1. Thêm `dartz` vào tệp `pubspec.yaml` của bạn dưới dependencies:
```yaml
dependencies:
  dartz: ^0.10.0
```

2. Sử dụng `Either` để xử lý lỗi một cách duyên dáng trong mã Dart của bạn:
```dart
import 'package:dartz/dartz.dart';

Either<String, int> divide(int dividend, int divisor) {
  if (divisor == 0) {
    return Left('Không thể chia cho không.');
  } else {
    return Right(dividend ~/ divisor);
  }
}

void main() {
  final result = divide(100, 0);
  result.fold(
    (left) => print('Lỗi: $left'), 
    (right) => print('Kết quả: $right')
  );
}
```
Kết quả mẫu: `Lỗi: Không thể chia cho không.`

Phần `Left` thường đại diện cho lỗi, và phần `Right` đại diện cho thành công. Mẫu này cho phép xử lý lỗi một cách hàm mô học hơn, mang lại sự rõ ràng và kiểm soát hơn trong quản lý lỗi.

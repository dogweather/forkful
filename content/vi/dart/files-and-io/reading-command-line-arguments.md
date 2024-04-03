---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:07.969026-07:00
description: "L\xE0m th\u1EBF n\xE0o: Dart cung c\u1EA5p m\u1ED9t c\xE1ch ti\u1EBF\
  p c\u1EADn \u0111\u01A1n gi\u1EA3n \u0111\u1EC3 truy c\u1EADp v\xE0o c\xE1c \u0111\
  \u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh th\xF4ng qua `List<String> args` trong ph\u01B0\
  \u01A1ng th\u1EE9c main. D\u01B0\u1EDBi \u0111\xE2y\u2026"
lastmod: '2024-03-13T22:44:36.283586-06:00'
model: gpt-4-0125-preview
summary: "Dart cung c\u1EA5p m\u1ED9t c\xE1ch ti\u1EBFp c\u1EADn \u0111\u01A1n gi\u1EA3\
  n \u0111\u1EC3 truy c\u1EADp v\xE0o c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7\
  nh th\xF4ng qua `List<String> args` trong ph\u01B0\u01A1ng th\u1EE9c main."
title: "\u0110\u1ECDc c\xE1c tham s\u1ED1 d\xF2ng l\u1EC7nh"
weight: 23
---

## Làm thế nào:
Dart cung cấp một cách tiếp cận đơn giản để truy cập vào các đối số dòng lệnh thông qua `List<String> args` trong phương thức main. Dưới đây là một ví dụ đơn giản minh họa cách đọc và sử dụng các đối số dòng lệnh.

```dart
// main.dart
void main(List<String> args) {
  print('Đối Số Dòng Lệnh:');
  for (var i = 0; i < args.length; i++) {
    print('${i + 1}: ${args[i]}');
  }
}
```

Để chạy chương trình Dart này và truyền các đối số dòng lệnh, sử dụng Dart CLI như sau:

```shell
dart run main.dart Hello World!
```

Kết quả mong đợi:

```
Đối Số Dòng Lệnh:
1: Hello
2: World!
```

### Sử dụng Một Thư Viện Bên Thứ Ba Phổ Biến: `args`
Mặc dù các khả năng tích hợp sẵn của Dart trong việc xử lý các đối số dòng lệnh là đủ mạnh mẽ cho nhiều ứng dụng, gói `args` cung cấp một cách tinh tế hơn để định nghĩa và phân tích các đối số dòng lệnh cho những nhu cầu phức tạp hơn.

Đầu tiên, thêm gói `args` vào `pubspec.yaml` của bạn:

```yaml
dependencies:
  args: ^2.0.0
```

Sau đó, sử dụng nó trong chương trình của bạn như sau:

```dart
// Sử dụng gói 'args'
import 'package:args/args.dart';

void main(List<String> arguments) {
  final parser = ArgParser()..addOption('name', abbr: 'n');
  final argResults = parser.parse(arguments);

  if (argResults.wasParsed('name')) {
    print('Xin chào, ${argResults['name']}!');
  } else {
    print('Không có tên được cung cấp.');
  }
}
```

Chạy chương trình với một đối số được đặt tên:

```shell
dart run main.dart --name=John
```

Kết quả mong đợi:

```
Xin chào, John!
```

Giới thiệu đơn giản này về cách phân tích các đối số dòng lệnh, cả natively và với thư viện `args`, thể hiện cách Dart có thể xử lý đầu vào từ người dùng ngay từ console, mở ra một con đường tạo ra các ứng dụng CLI tương tác và động hơn.

---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:56.515646-07:00
description: "L\xE0m th\u1EBF n\xE0o: Dart kh\xF4ng \u0111i k\xE8m v\u1EDBi REPL \u0111\
  \u01B0\u1EE3c t\xEDch h\u1EE3p s\u1EB5n. Tuy nhi\xEAn, b\u1EA1n c\xF3 th\u1EC3 \u0111\
  \u1EA1t \u0111\u01B0\u1EE3c ch\u1EE9c n\u0103ng t\u01B0\u01A1ng t\u1EF1 REPL b\u1EB1\
  ng c\xE1ch s\u1EED d\u1EE5ng DartPad (tr\u1EF1c tuy\u1EBFn)\u2026"
lastmod: '2024-03-13T22:44:36.265097-06:00'
model: gpt-4-0125-preview
summary: "Dart kh\xF4ng \u0111i k\xE8m v\u1EDBi REPL \u0111\u01B0\u1EE3c t\xEDch h\u1EE3\
  p s\u1EB5n."
title: "S\u1EED d\u1EE5ng giao di\u1EC7n d\xF2ng l\u1EC7nh t\u01B0\u01A1ng t\xE1c\
  \ (REPL)"
weight: 34
---

## Làm thế nào:
Dart không đi kèm với REPL được tích hợp sẵn. Tuy nhiên, bạn có thể đạt được chức năng tương tự REPL bằng cách sử dụng DartPad (trực tuyến) hoặc bằng cách sử dụng các công cụ bên thứ ba như `dart_repl`.

**Sử dụng DartPad:**

DartPad (https://dartpad.dev) là một trình biên soạn Dart trực tuyến cho phép bạn viết và chạy mã Dart trong trình duyệt web của mình. Mặc dù không phải là một REPL dòng lệnh truyền thống, nhưng nó cung cấp một trải nghiệm tương tự cho việc thử nghiệm nhanh chóng.

Chỉ cần truy cập vào trang web, nhập mã Dart của bạn vào khung bên trái và nhấn "Run" để xem kết quả hiển thị bên phải.

Ví dụ:
```dart
void main() {
  print('Xin chào, Dart!');
}
```
Kết quả:
```
Xin chào, Dart!
```

**Sử dụng `dart_repl` (công cụ bên thứ ba):**

Đầu tiên, cài đặt `dart_repl` thông qua pub một cách toàn cầu:

```shell
dart pub global activate dart_repl
```

Sau đó, chạy `dart_repl` từ terminal của bạn:

```shell
dart_repl
```

Bây giờ, bạn có thể bắt đầu nhập các câu lệnh Dart trực tiếp vào shell. Ví dụ:

```dart
>>> print('Xin chào, REPL!');
Xin chào, REPL!
>>> int add(int x, int y) => x + y;
>>> print(add(5, 7));
12
```

Những phương pháp này cung cấp một con đường nhanh chóng để thử nghiệm mã Dart ngay lập tức, giúp giảm đáng kể độ dốc của quá trình học và tăng cường năng suất.

---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:07.533362-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Dart, n\u1ED9i suy chu\u1ED7i kh\xE1 \u0111\
  \u01A1n gi\u1EA3n, s\u1EED d\u1EE5ng k\xFD t\u1EF1 `$` \u0111\u1EC3 n\u1ED9i suy\
  \ c\xE1c bi\u1EC3u th\u1EE9c tr\u1EF1c ti\u1EBFp trong c\xE1c chu\u1ED7i k\xFD t\u1EF1\
  ."
lastmod: '2024-03-13T22:44:36.243739-06:00'
model: gpt-4-0125-preview
summary: "Trong Dart, n\u1ED9i suy chu\u1ED7i kh\xE1 \u0111\u01A1n gi\u1EA3n, s\u1EED\
  \ d\u1EE5ng k\xFD t\u1EF1 `$` \u0111\u1EC3 n\u1ED9i suy c\xE1c bi\u1EC3u th\u1EE9\
  c tr\u1EF1c ti\u1EBFp trong c\xE1c chu\u1ED7i k\xFD t\u1EF1."
title: "N\u1ED9i suy chu\u1ED7i k\xFD t\u1EF1"
weight: 8
---

## Làm thế nào:
Trong Dart, nội suy chuỗi khá đơn giản, sử dụng ký tự `$` để nội suy các biểu thức trực tiếp trong các chuỗi ký tự:

```dart
void main() {
  String name = 'Dart';
  int year = 2023;
  // Nội suy biến đơn giản
  print('Học $name vào năm $year!');
  // Kết quả: Học Dart vào năm 2023!
  
  // Nội suy biểu thức
  print('Trong hai năm nữa, sẽ là năm ${year + 2}.');
  // Kết quả: Trong hai năm nữa, sẽ là năm 2025.
}
```

Trong trường hợp bạn có các biểu thức phức tạp hơn hoặc muốn thực hiện các phép toán ngay trong chuỗi, hãy đóng biểu thức trong `${}`. Dart không có bất kỳ thư viện bên thứ ba phổ biến nào cụ thể cho nội suy chuỗi vì nó đã được trang bị đầy đủ từ nội bộ để xử lý các tình huống đa dạng và phức tạp.

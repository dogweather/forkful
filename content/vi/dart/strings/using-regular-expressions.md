---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:24.730737-07:00
description: "L\xE0m sao: Dart s\u1EED d\u1EE5ng l\u1EDBp `RegExp` cho bi\u1EC3u th\u1EE9\
  c ch\xEDnh quy. D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t v\xED d\u1EE5 c\u01A1\
  \ b\u1EA3n \u0111\u1EC3 t\xECm m\u1ED9t m\u1EABu \u0111\u01A1n gi\u1EA3n trong m\u1ED9\
  t chu\u1ED7i."
lastmod: '2024-03-13T22:44:36.249035-06:00'
model: gpt-4-0125-preview
summary: "Dart s\u1EED d\u1EE5ng l\u1EDBp `RegExp` cho bi\u1EC3u th\u1EE9c ch\xED\
  nh quy."
title: "S\u1EED d\u1EE5ng bi\u1EC3u th\u1EE9c ch\xEDnh quy"
weight: 11
---

## Làm sao:
Dart sử dụng lớp `RegExp` cho biểu thức chính quy. Dưới đây là một ví dụ cơ bản để tìm một mẫu đơn giản trong một chuỗi:

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Học lập trình Dart thật thú vị.';

  if (pattern.hasMatch(text)) {
    print('Tìm thấy kết quả khớp!');
  } else {
    print('Không tìm thấy kết quả khớp.');
  }
  // Kết quả: Tìm thấy kết quả khớp!
}
```

Để trích xuất các kết quả khớp từ một chuỗi, bạn có thể sử dụng phương thức `allMatches`. Phương thức này trả về một đối tượng lặp của các kết quả khớp:

```dart
void main() {
  var pattern = RegExp(r'\b\w+\b');
  var text = 'Dart thật tuyệt vời!';

  var matches = pattern.allMatches(text);
  for (final match in matches) {
    print(match.group(0)); // Điều này in ra các chuỗi con khớp.
  }
  // Kết quả:
  // Dart
  // thật
  // tuyệt vời
}
```

Thay thế văn bản có thể được thực hiện bằng cách sử dụng các phương thức `replaceFirst` hoặc `replaceAll`:

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Dart không chỉ là một cái dart.';
  
  // Thay thế lần xuất hiện đầu tiên
  var modifiedText = text.replaceFirst(pattern, 'Flutter');
  print(modifiedText); 
  // Kết quả: Flutter không chỉ là một cái dart.

  // Thay thế tất cả các lần xuất hiện
  modifiedText = text.replaceAll(pattern, 'Flutter');
  print(modifiedText);
  // Kết quả: Flutter không chỉ là một cái flutter.
}
```

Phân chia một chuỗi bằng một mẫu regex rất đơn giản khi sử dụng phương thức `split`:

```dart
void main() {
  var pattern = RegExp(r'\s+'); // Khớp với bất kỳ ký tự khoảng trắng nào
  var text = 'Dart thật vui';

  var parts = text.split(pattern);
  print(parts); 
  // Kết quả: [Dart, thật, vui]
}
```

Đối với việc phân tích cú pháp hoặc xác thực phức tạp không được Dart's `RegExp` hỗ trợ trực tiếp, bạn có thể xem xét sử dụng thư viện bên thứ ba, nhưng thư viện chuẩn của Dart thường đủ cho các tác vụ regex thông thường, nhấn mạnh tính hữu ích và linh hoạt của nó trong việc xử lý biểu thức chính quy.

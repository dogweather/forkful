---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:06.013299-07:00
description: "Vi\u1EC7c t\xECm \u0111\u1ED9 d\xE0i c\u1EE7a m\u1ED9t Chu\u1ED7i trong\
  \ Dart l\xE0 vi\u1EC7c x\xE1c \u0111\u1ECBnh s\u1ED1 \u0111\u01A1n v\u1ECB m\xE3\
  \ h\xF3a (th\u1EF1c ch\u1EA5t l\xE0 s\u1ED1 l\u01B0\u1EE3ng k\xFD t\u1EF1 n\u1EBF\
  u suy ngh\u0129 m\u1ED9t c\xE1ch \u0111\u01A1n gi\u1EA3n) trong m\u1ED9t Chu\u1ED7\
  i \u0111\xE3\u2026"
lastmod: '2024-03-13T22:44:36.250460-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\xECm \u0111\u1ED9 d\xE0i c\u1EE7a m\u1ED9t Chu\u1ED7i trong\
  \ Dart l\xE0 vi\u1EC7c x\xE1c \u0111\u1ECBnh s\u1ED1 \u0111\u01A1n v\u1ECB m\xE3\
  \ h\xF3a (th\u1EF1c ch\u1EA5t l\xE0 s\u1ED1 l\u01B0\u1EE3ng k\xFD t\u1EF1 n\u1EBF\
  u suy ngh\u0129 m\u1ED9t c\xE1ch \u0111\u01A1n gi\u1EA3n) trong m\u1ED9t Chu\u1ED7\
  i \u0111\xE3\u2026"
title: "T\xECm \u0111\u1ED9 d\xE0i c\u1EE7a chu\u1ED7i"
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Việc tìm độ dài của một Chuỗi trong Dart là việc xác định số đơn vị mã hóa (thực chất là số lượng ký tự nếu suy nghĩ một cách đơn giản) trong một Chuỗi đã cho. Các lập trình viên thực hiện điều này để có thể thao tác chuỗi một cách chính xác hơn, chẳng hạn như xác thực đầu vào, cắt bớt văn bản hiển thị, hoặc xử lý các định dạng dữ liệu mà ở đó độ dài có ý nghĩa (ví dụ, các giao thức với thông điệp có tiền tố độ dài).

## Nên làm thế nào:
Dart làm cho việc lấy độ dài của một chuỗi trở nên rất đơn giản bằng cách sử dụng thuộc tính `length`. Dưới đây là một ví dụ cơ bản:

```dart
void main() {
  String myString = "Hello, Dart!";
  print("Độ dài của '\(myString)' là: \(myString.length)");
  // Kết quả: Độ dài của 'Hello, Dart!' là: 12
}
```
Thuộc tính này đếm số đơn vị mã hóa UTF-16 trong chuỗi, tương ứng với độ dài của chuỗi cho hầu hết các trường hợp sử dụng phổ biến.

Đối với việc xử lý văn bản tinh tế hơn, nhất là khi liên quan đến các ký tự Unicode nằm ngoài Phạm vi Đa Ngôn Ngữ Cơ Bản (BMP), hãy cân nhắc sử dụng gói `characters` để đếm các cụm đồ thị, điều này đại diện một cách chính xác hơn cho những ký tự mà người dùng cảm nhận được.

Trước tiên, thêm `characters` vào `pubspec.yaml` của bạn:

```yaml
dependencies:
  characters: ^1.2.0
```

Sau đó, sử dụng như sau:

```dart
import 'package:characters/characters.dart';

void main() {
  String myEmojiString = "👨‍👩‍👧‍👦 gia đình";
  print("Độ dài của '\(myEmojiString)' là: \(myEmojiString.characters.length)");
  // Kết quả: Độ dài của '👨‍👩‍👧‍👦 gia đình' là: 8
}
```

Trong ví dụ này, `myEmojiString.characters.length` cho chúng ta độ dài theo các cụm đồ thị Unicode, đây là một biểu diễn chính xác hơn cho các chuỗi chứa các ký tự phức tạp, như emojis hay các dấu kết hợp ký tự.

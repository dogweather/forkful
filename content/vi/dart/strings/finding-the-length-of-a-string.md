---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:06.013299-07:00
description: "N\xEAn l\xE0m th\u1EBF n\xE0o: Dart l\xE0m cho vi\u1EC7c l\u1EA5y \u0111\
  \u1ED9 d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i tr\u1EDF n\xEAn r\u1EA5t \u0111\u01A1\
  n gi\u1EA3n b\u1EB1ng c\xE1ch s\u1EED d\u1EE5ng thu\u1ED9c t\xEDnh `length`. D\u01B0\
  \u1EDBi \u0111\xE2y l\xE0 m\u1ED9t v\xED d\u1EE5 c\u01A1 b\u1EA3n."
lastmod: '2024-03-13T22:44:36.250460-06:00'
model: gpt-4-0125-preview
summary: "Dart l\xE0m cho vi\u1EC7c l\u1EA5y \u0111\u1ED9 d\xE0i c\u1EE7a m\u1ED9\
  t chu\u1ED7i tr\u1EDF n\xEAn r\u1EA5t \u0111\u01A1n gi\u1EA3n b\u1EB1ng c\xE1ch\
  \ s\u1EED d\u1EE5ng thu\u1ED9c t\xEDnh `length`."
title: "T\xECm \u0111\u1ED9 d\xE0i c\u1EE7a chu\u1ED7i"
weight: 7
---

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

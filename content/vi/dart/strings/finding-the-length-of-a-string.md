---
title:                "Tìm độ dài của chuỗi"
date:                  2024-03-08T21:55:06.013299-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
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

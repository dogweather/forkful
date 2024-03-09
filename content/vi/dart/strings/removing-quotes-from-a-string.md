---
title:                "Loại bỏ dấu ngoặc kép khỏi chuỗi"
date:                  2024-03-08T21:56:03.995891-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Lý do & Tại sao?
Việc loại bỏ dấu ngoặc từ một chuỗi trong Dart bao gồm việc gỡ bỏ dấu ngoặc kép (") hoặc dấu ngoặc đơn (') ở đầu và cuối của một chuỗi, hữu ích cho việc làm sạch dữ liệu hoặc chuẩn bị chuỗi cho việc xử lý tiếp theo. Các lập trình viên thực hiện việc này để chuẩn hoá đầu vào dữ liệu, đảm bảo sự thống nhất trong việc lưu trữ dữ liệu, hoặc khi giao tiếp với các API có thể trả về dữ liệu dưới dạng có ngoặc.

## Cách thực hiện:
Dart cung cấp cách đơn giản để loại bỏ dấu ngoặc từ chuỗi bằng các phương thức chuỗi có sẵn mà không cần tới thư viện bên thứ ba.

### Ví dụ 1: Sử dụng `replaceFirst` và `replaceAll`
Nếu bạn đang xử lý các chuỗi bắt đầu và kết thúc bằng dấu ngoặc, bạn có thể sử dụng các phương thức `replaceFirst` và `replaceAll` để loại bỏ chúng.

```dart
String quotedString = '"Hello, World!"';
String singleQuotedString = '\'Dart Programming\'';

// Loại bỏ dấu ngoặc kép
String noDoubleQuotes = quotedString.replaceFirst('"', '').replaceAll('"', '');
print(noDoubleQuotes); // Kết quả: Hello, World!

// Loại bỏ dấu ngoặc đơn
String noSingleQuotes = singleQuotedString.replaceFirst('\'', '').replaceAll('\'', '');
print(noSingleQuotes); // Kết quả: Dart Programming
```

### Ví dụ 2: Sử dụng `substring`
Phương thức này hữu ích khi bạn chắc chắn rằng các dấu ngoặc nằm ngay ở đầu và cuối chuỗi.

```dart
String quotedString = '"Flutter Development"';
// Kiểm tra nếu nó bắt đầu và kết thúc bằng dấu ngoặc trước khi loại bỏ để tránh lỗi
if (quotedString.startsWith('"') && quotedString.endsWith('"')) {
  quotedString = quotedString.substring(1, quotedString.length - 1);
}
print(quotedString); // Kết quả: Flutter Development
```

### Ví dụ 3: Phương thức Mở rộng Tùy chỉnh
Để tái sử dụng nhiều hơn, đặc biệt nếu dự án của bạn thường xuyên liên quan đến việc loại bỏ dấu ngoặc, hãy cân nhắc tạo một phương thức mở rộng tùy chỉnh trên `String`.

```dart
extension UnquoteString on String {
  String unquote() {
    var str = this;
    if (str.startsWith('"') && str.endsWith('"') || str.startsWith('\'') && str.endsWith('\'')) {
      str = str.substring(1, str.length - 1);
    }
    return str;
  }
}

void main() {
  String doubleQuoted = '"This is Dart"';
  String singleQuoted = '\'This is awesome\'';
  print(doubleQuoted.unquote()); // Kết quả: This is Dart
  print(singleQuoted.unquote()); // Kết quả: This is awesome
}
```

Những cách tiếp cận này sẽ giúp bạn loại bỏ dấu ngoặc từ chuỗi một cách hiệu quả trong Dart, nâng cao quy trình xử lý và chuẩn bị dữ liệu của bạn.

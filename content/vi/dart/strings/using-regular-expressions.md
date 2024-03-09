---
title:                "Sử dụng biểu thức chính quy"
date:                  2024-03-08T21:57:24.730737-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Biểu thức chính quy (regex) trong Dart cung cấp một cách mạnh mẽ để tìm kiếm và thao tác với chuỗi ký tự, cho phép lập trình viên thực hiện các tác vụ xử lý văn bản phức tạp một cách hiệu quả. Bằng cách hiểu biểu thức chính quy, các nhà phát triển có thể thực hiện kiểm tra văn bản, tìm kiếm mẫu và biến đổi văn bản nhanh chóng, điều này rất cần thiết cho việc xử lý biểu mẫu, phân tích dữ liệu và thao tác chuỗi chung trong ứng dụng hiện đại.

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

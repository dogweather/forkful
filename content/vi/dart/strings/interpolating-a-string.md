---
title:                "Nội suy chuỗi ký tự"
date:                  2024-03-08T21:55:07.533362-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Nội suy chuỗi là quá trình chèn trực tiếp giá trị biến vào trong chuỗi, thường được dùng để tạo ra các thông điệp ý nghĩa mà không cần đến sự nối chuỗi rườm rà. Lập trình viên thực hiện điều này để làm cho mã nguồn sạch hơn, dễ đọc hơn và ngăn chặn các lỗi có khả năng xảy ra trong quá trình nối chuỗi phức tạp.

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

---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:48.043131-07:00
description: '#'
lastmod: '2024-03-09T21:06:00.988249-07:00'
model: gpt-4-0125-preview
summary: '#'
title: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n"
---

{{< edit_this_page >}}

## Tìm kiếm và thay thế văn bản với Dart

### Lý do và Lợi ích

Tìm kiếm và thay thế văn bản trong Dart bao gồm việc kiểm tra các chuỗi để tìm kiếm các mẫu hoặc chuỗi ký tự nhất định và thay thế chúng bằng nội dung mới. Thao tác này là cơ bản cho các nhiệm vụ như kiểm tra dữ liệu, định dạng đầu ra, phân tích đầu vào của người dùng, hoặc thậm chí là manipul thông tin URLs và đường dẫn file, làm cho ứng dụng trở nên linh hoạt và phản hồi với nhu cầu của người dùng.

### Cách thực hiện:

Dart cung cấp các phương thức mạnh mẽ để tìm kiếm và thay thế văn bản trực tiếp thông qua lớp `String` của nó, mà không cần đến các thư viện bên ngoài. Đây là cách bạn có thể thực hiện nó:

#### Tìm kiếm và Thay thế Cơ bản

Để tìm kiếm một chuỗi con và thay thế nó bằng một chuỗi khác, bạn có thể sử dụng `replaceAll`:

```dart
String sampleText = "Hello, Dart! Dart là tuyệt vời.";
String modifiedText = sampleText.replaceAll("Dart", "Flutter");
print(modifiedText); // Kết quả: Hello, Flutter! Flutter là tuyệt vời.
```

#### Sử dụng Biểu thức Chính quy

Đối với nhu cầu tìm kiếm và thay thế phức tạp hơn, Dart sử dụng biểu thức chính quy thông qua lớp `RegExp`. Điều này cho phép tìm kiếm và thay thế mẫu trong chuỗi:

```dart
String sampleText = "Dart 2023, Flutter 2023";
String modifiedText = sampleText.replaceAll(RegExp(r'\d+'), "2024");
print(modifiedText); // Kết quả: Dart 2024, Flutter 2024
```

Ví dụ này tìm tất cả các ví dụ về một hoặc nhiều chữ số (`\d+`) trong chuỗi và thay thế chúng bằng "2024".

#### Tìm kiếm Không phân biệt chữ Hoa chữ Thường

Để thực hiện tìm kiếm không phân biệt chữ hoa chữ thường, bạn có thể thay đổi bộ xây dựng `RegExp` để bỏ qua chữ hoa chữ thường:

```dart
String sampleText = "Chào mừng đến với Dart, ngôn ngữ lập trình.";
String modifiedText = sampleText.replaceAll(RegExp(r'dart', caseSensitive: false), "Flutter");
print(modifiedText); // Kết quả: Chào mừng đến với Flutter, ngôn ngữ lập trình.
```

#### Thay thế bằng một Hàm

Đối với việc thay thế động dựa trên chính sự khớp được tìm thấy, Dart cho phép truyền một hàm vào `replaceAllMapped`. Hàm này có thể thực hiện các thao tác hoặc tính toán trên các chuỗi khớp được tìm thấy:

```dart
String sampleText = "Tăng 5 lên 1 để được 6.";
String incrementedText = sampleText.replaceAllMapped(RegExp(r'\d+'), (Match m) => (int.parse(m[0]!) + 1).toString());
print(incrementedText); // Kết quả: Tăng 6 lên 1 để được 7.
```

Điều này thay thế mỗi chuỗi số với giá trị tăng của nó. Mỗi khớp được phân tích thành một số nguyên, tăng lên và sau đó được chuyển đổi trở lại thành một chuỗi để thay thế.

Khả năng manipul chuỗi của Dart, đặc biệt là với tìm kiếm và thay thế văn bản, làm cho nó trở thành một công cụ mạnh mẽ cho việc xử lý và chuẩn bị dữ liệu trong ứng dụng của bạn. Cho dù sử dụng thay thế chuỗi đơn giản hay tận dụng sức mạnh của biểu thức chính quy, Dart cung cấp sự linh hoạt và hiệu suất cần thiết cho việc manipul văn bản hiệu quả.

---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:31.346075-07:00
description: "Vi\u1EC7c ph\xE2n t\xEDch (parsing) m\u1ED9t ng\xE0y t\u1EEB m\u1ED9\
  t chu\u1ED7i trong Dart li\xEAn quan \u0111\u1EBFn vi\u1EC7c chuy\u1EC3n \u0111\u1ED5\
  i bi\u1EC3u di\u1EC5n v\u0103n b\u1EA3n c\u1EE7a ng\xE0y v\xE0 gi\u1EDD th\xE0nh\
  \ m\u1ED9t \u0111\u1ED1i t\u01B0\u1EE3ng `DateTime`.\u2026"
lastmod: '2024-03-13T22:44:36.275610-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c ph\xE2n t\xEDch (parsing) m\u1ED9t ng\xE0y t\u1EEB m\u1ED9t chu\u1ED7\
  i trong Dart li\xEAn quan \u0111\u1EBFn vi\u1EC7c chuy\u1EC3n \u0111\u1ED5i bi\u1EC3\
  u di\u1EC5n v\u0103n b\u1EA3n c\u1EE7a ng\xE0y v\xE0 gi\u1EDD th\xE0nh m\u1ED9t\
  \ \u0111\u1ED1i t\u01B0\u1EE3ng `DateTime`.\u2026"
title: "Ph\xE2n t\xEDch ng\xE0y t\u1EEB chu\u1ED7i k\xFD t\u1EF1"
weight: 30
---

## Cái gì & Tại sao?
Việc phân tích (parsing) một ngày từ một chuỗi trong Dart liên quan đến việc chuyển đổi biểu diễn văn bản của ngày và giờ thành một đối tượng `DateTime`. Thao tác này rất cần thiết cho các ứng dụng liên quan đến lịch trình, phân tích dữ liệu, hoặc bất kỳ tính năng nào đòi hỏi sự thao tác với ngày, đảm bảo rằng dữ liệu liên quan đến ngày được hiểu và xử lý đúng cách bởi chương trình.

## Làm thế nào:
Thư viện cốt lõi của Dart đơn giản hóa việc phân tích ngày qua lớp `DateTime`. Đối với các trường hợp đơn giản khi bạn biết định dạng của chuỗi ngày, bạn có thể sử dụng phương thức `DateTime.parse()`. Tuy nhiên, đối với các tình huống phức tạp hơn hoặc khi xử lý nhiều định dạng, gói `intl`, cụ thể là lớp `DateFormat`, trở nên vô giá.

### Sử dụng Thư Viện Cốt Lõi Dart:
```dart
void main() {
  // Sử dụng DateTime.parse()
  var dateString = "2023-10-31";
  var parsedDate = DateTime.parse(dateString);
  
  print(parsedDate); // 2023-10-31 00:00:00.000
}
```

### Sử dụng Gói `intl`:
Đầu tiên, thêm gói `intl` vào tệp `pubspec.yaml` của bạn:
```yaml
dependencies:
  intl: ^0.17.0
```
Sau đó, nhập gói và sử dụng `DateFormat` để phân tích:
```dart
import 'package:intl/intl.dart';

void main() {
  var dateString = "October 31, 2023";
  var dateFormat = DateFormat("MMMM dd, yyyy");
  var parsedDate = dateFormat.parse(dateString);
  
  print(parsedDate); // 2023-10-31 00:00:00.000
}
```
Gói `intl` cung cấp các tùy chọn mạnh mẽ cho việc phân tích ngày, cho phép xử lý một cách mượt mà các định dạng ngày quốc tế khác nhau.

---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:08.952702-07:00
description: "L\xE0m th\u1EBF n\xE0o: Dart cung c\u1EA5p s\u1EF1 h\u1ED7 tr\u1EE3\
  \ m\u1EA1nh m\u1EBD cho vi\u1EC7c thao t\xE1c v\u1EDBi ng\xE0y th\xE1ng th\xF4ng\
  \ qua l\u1EDBp `DateTime` c\u1EE7a m\xECnh. D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1\
  ch b\u1EA1n c\xF3 th\u1EC3 t\xEDnh to\xE1n\u2026"
lastmod: '2024-03-13T22:44:36.280909-06:00'
model: gpt-4-0125-preview
summary: "Dart cung c\u1EA5p s\u1EF1 h\u1ED7 tr\u1EE3 m\u1EA1nh m\u1EBD cho vi\u1EC7\
  c thao t\xE1c v\u1EDBi ng\xE0y th\xE1ng th\xF4ng qua l\u1EDBp `DateTime` c\u1EE7\
  a m\xECnh."
title: "T\xEDnh to\xE1n ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7c qu\xE1 kh\u1EE9"
weight: 26
---

## Làm thế nào:
Dart cung cấp sự hỗ trợ mạnh mẽ cho việc thao tác với ngày tháng thông qua lớp `DateTime` của mình. Dưới đây là cách bạn có thể tính toán ngày trong tương lai hoặc quá khứ bằng Dart gốc, không cần đến thư viện bên thứ ba.

### Tính toán Ngày Tương Lai
Để tính toán một ngày trong tương lai, bạn tạo một đối tượng `DateTime` và sử dụng phương thức `add` với thời lượng mong muốn.

```dart
DateTime today = DateTime.now();
Duration tenDays = Duration(days: 10);
DateTime futureDate = today.add(tenDays);

print(futureDate); // Kết quả: 2023-04-21 14:22:35.123456 (ví dụ về kết quả, tùy thuộc vào ngày và thời gian hiện tại)
```

### Tính toán Ngày Quá Khứ
Để tính toán một ngày trong quá khứ, bạn sử dụng phương thức `subtract` trên một đối tượng `DateTime` với thời lượng cần thiết.

```dart
DateTime today = DateTime.now();
Duration fifteenDaysAgo = Duration(days: 15);
DateTime pastDate = today.subtract(fifteenDaysAgo);

print(pastDate); // Kết quả: 2023-03-27 14:22:35.123456 (ví dụ về kết quả, tùy thuộc vào ngày và thời gian hiện tại)
```

### Sử dụng Thư Viện Bên Thứ Ba
Mặc dù khả năng thao tác với ngày tháng bản địa của Dart rất mạnh mẽ, bạn có thể thấy mình cần thêm các thao tác cụ thể hơn, như phân tích cú pháp hoặc định dạng ngày tháng dễ dàng hơn, hoặc thực hiện các tính toán phức tạp. Trong những trường hợp như vậy, gói `time` có thể rất hữu ích.

Đầu tiên, thêm `time` vào phụ thuộc `pubspec.yaml` của bạn:

```yaml
dependencies:
  time: ^2.0.0
```

Sau đó, bạn có thể sử dụng nó để thực hiện các phép tính tương tự với độ đọc cao hơn:

```dart
import 'package:time/time.dart';

void main() {
  DateTime today = DateTime.now();

  // Tính toán ngày tương lai
  DateTime futureDate = today + 10.days;
  print(futureDate); // Định dạng kết quả: 2023-04-21 14:22:35.123456

  // Tính toán ngày quá khứ
  DateTime pastDate = today - 15.days;
  print(pastDate); // Định dạng kết quả: 2023-03-27 14:22:35.123456
}
```

Những ví dụ này minh họa cách thao tác cơ bản với ngày tháng trong Dart, bao gồm việc cộng và trừ thời gian vào hoặc từ một ngày hiện tại, cho thấy việc quản lý ngày tháng trong các ứng dụng Dart diễn ra một cách dễ dàng.

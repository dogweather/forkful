---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:08.952702-07:00
description: "Vi\u1EC7c t\xEDnh to\xE1n m\u1ED9t ng\xE0y trong t\u01B0\u01A1ng lai\
  \ ho\u1EB7c qu\xE1 kh\u1EE9 l\xE0 m\u1ED9t nhi\u1EC7m v\u1EE5 ph\u1ED5 bi\u1EBF\
  n \u0111\u1ED1i v\u1EDBi c\xE1c l\u1EADp tr\xECnh vi\xEAn, \u0111\u1ED1i ph\xF3\
  \ v\u1EDBi vi\u1EC7c l\u1EADp l\u1ECBch, nh\u1EAFc nh\u1EDF, ho\u1EB7c b\u1EA5t\
  \ k\u1EF3\u2026"
lastmod: '2024-03-13T22:44:36.280909-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\xEDnh to\xE1n m\u1ED9t ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7\
  c qu\xE1 kh\u1EE9 l\xE0 m\u1ED9t nhi\u1EC7m v\u1EE5 ph\u1ED5 bi\u1EBFn \u0111\u1ED1\
  i v\u1EDBi c\xE1c l\u1EADp tr\xECnh vi\xEAn, \u0111\u1ED1i ph\xF3 v\u1EDBi vi\u1EC7\
  c l\u1EADp l\u1ECBch, nh\u1EAFc nh\u1EDF, ho\u1EB7c b\u1EA5t k\u1EF3 t\xEDnh n\u0103\
  ng n\xE0o ph\u1EE5 thu\u1ED9c v\xE0o vi\u1EC7c t\xEDnh to\xE1n ng\xE0y th\xE1ng."
title: "T\xEDnh to\xE1n ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7c qu\xE1 kh\u1EE9"
weight: 26
---

## Cái gì & Tại sao?
Việc tính toán một ngày trong tương lai hoặc quá khứ là một nhiệm vụ phổ biến đối với các lập trình viên, đối phó với việc lập lịch, nhắc nhở, hoặc bất kỳ tính năng nào phụ thuộc vào việc tính toán ngày tháng. Hiểu cách thao tác với ngày tháng là điều quan trọng đối với hệ thống backend, giao diện người dùng và phân tích dữ liệu, đặc biệt là đối với những người chuyển sang Dart và muốn thực hiện logic thời gian một cách hiệu quả.

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

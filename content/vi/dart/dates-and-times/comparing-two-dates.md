---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:59.874950-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Dart, b\u1EA1n c\xF3 th\u1EC3 so s\xE1\
  nh c\xE1c ng\xE0y s\u1EED d\u1EE5ng l\u1EDBp `DateTime`, c\xF3 c\xE1c ph\u01B0\u01A1\
  ng th\u1EE9c nh\u01B0 `isBefore`, `isAfter`, v\xE0 `isAtSameMomentAs` \u0111\u1EC3\
  \ so\u2026"
lastmod: '2024-03-13T22:44:36.279574-06:00'
model: gpt-4-0125-preview
summary: "Trong Dart, b\u1EA1n c\xF3 th\u1EC3 so s\xE1nh c\xE1c ng\xE0y s\u1EED d\u1EE5\
  ng l\u1EDBp `DateTime`, c\xF3 c\xE1c ph\u01B0\u01A1ng th\u1EE9c nh\u01B0 `isBefore`,\
  \ `isAfter`, v\xE0 `isAtSameMomentAs` \u0111\u1EC3 so s\xE1nh tr\u1EF1c ti\u1EBF\
  p."
title: "So s\xE1nh hai ng\xE0y"
weight: 27
---

## Làm thế nào:
Trong Dart, bạn có thể so sánh các ngày sử dụng lớp `DateTime`, có các phương thức như `isBefore`, `isAfter`, và `isAtSameMomentAs` để so sánh trực tiếp. Bên cạnh đó, sự khác biệt giữa các ngày có thể được xác định sử dụng phương thức `difference()`, cung cấp một đối tượng `Duration` chi tiết khoảng thời gian giữa hai điểm thời gian.

Dưới đây là một ví dụ cơ bản minh họa các khái niệm này:

```dart
void main() {
  DateTime eventStart = DateTime(2023, 5, 15);
  DateTime eventEnd = DateTime(2023, 5, 20);
  
  // Kiểm tra xem một ngày có trước ngày khác không
  if (eventStart.isBefore(eventEnd)) {
    print("Ngày bắt đầu sự kiện trước ngày kết thúc sự kiện.");
  }

  // Kiểm tra nếu hai ngày là giống nhau
  if (!eventStart.isAtSameMomentAs(eventEnd)) {
    print("Ngày bắt đầu và kết thúc sự kiện không giống nhau.");
  }
  
  // Tính toán sự khác biệt giữa hai ngày
  Duration eventDuration = eventEnd.difference(eventStart);
  print("Sự kiện kéo dài trong ${eventDuration.inDays} ngày.");
}

/*
Đầu ra:
Ngày bắt đầu sự kiện trước ngày kết thúc sự kiện.
Ngày bắt đầu và kết thúc sự kiện không giống nhau.
Sự kiện kéo dài trong 5 ngày.
*/
```

Đối với các thao tác với ngày nâng cao hơn, chẳng hạn như chuyển đổi định dạng, bạn có thể thấy lớp `DateFormat` từ gói `intl` hữu ích. Dưới đây là một ví dụ minh họa cách sử dụng nó để định dạng và so sánh các ngày:

Đầu tiên, bao gồm gói `intl` trong tệp `pubspec.yaml` của bạn:

```yaml
dependencies:
  intl: ^0.17.0
```

Sau đó, sử dụng nó như sau:

```dart
import 'package:intl/intl.dart';

void main() {
  DateTime departureDate = DateTime(2023, 5, 15);
  DateTime returnDate = DateTime.parse('2023-05-20');

  // Định dạng các ngày
  var formatter = DateFormat('yyyy-MM-dd');
  print("Khởi hành: ${formatter.format(departureDate)}");
  print("Trở về: ${formatter.format(returnDate)}");

  // So sánh sử dụng chuỗi đã định dạng
  if (formatter.format(departureDate) == formatter.format(returnDate)) {
    print("Ngày khởi hành và trở về giống nhau.");
  } else {
    print("Ngày khởi hành và trở về khác nhau.");
  }
}

/*
Đầu ra:
Khởi hành: 2023-05-15
Trở về: 2023-05-20
Ngày khởi hành và trở về khác nhau.
*/
```

Ví dụ này giới thiệu cách so sánh hai đối tượng `DateTime` cả trực tiếp và bằng cách sử dụng chuỗi đã định dạng để so sánh khi cần bỏ qua các thành phần cụ thể như thời gian.

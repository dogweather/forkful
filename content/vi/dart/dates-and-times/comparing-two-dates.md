---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:59.874950-07:00
description: "So s\xE1nh hai ng\xE0y trong Dart bao g\u1ED3m vi\u1EC7c \u0111\xE1\
  nh gi\xE1 s\u1EF1 kh\xE1c bi\u1EC7t v\u1EC1 th\u1EDDi gian ho\u1EB7c th\u1EE9 t\u1EF1\
  \ gi\u1EEFa ch\xFAng, l\xE0 m\u1ED9t ch\u1EE9c n\u0103ng thi\u1EBFt y\u1EBFu trong\
  \ c\xE1c \u1EE9ng d\u1EE5ng qu\u1EA3n l\xFD\u2026"
lastmod: '2024-03-09T21:06:01.016445-07:00'
model: gpt-4-0125-preview
summary: "So s\xE1nh hai ng\xE0y trong Dart bao g\u1ED3m vi\u1EC7c \u0111\xE1nh gi\xE1\
  \ s\u1EF1 kh\xE1c bi\u1EC7t v\u1EC1 th\u1EDDi gian ho\u1EB7c th\u1EE9 t\u1EF1 gi\u1EEF\
  a ch\xFAng, l\xE0 m\u1ED9t ch\u1EE9c n\u0103ng thi\u1EBFt y\u1EBFu trong c\xE1c\
  \ \u1EE9ng d\u1EE5ng qu\u1EA3n l\xFD\u2026"
title: "So s\xE1nh hai ng\xE0y"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
So sánh hai ngày trong Dart bao gồm việc đánh giá sự khác biệt về thời gian hoặc thứ tự giữa chúng, là một chức năng thiết yếu trong các ứng dụng quản lý sự kiện, hạn chót hoặc bất kỳ dữ liệu nhạy cảm với thời gian nào. Lập trình viên thường cần điều này để kiểm soát luồng logic, xác thực hoặc sắp xếp dữ liệu dựa trên điều kiện thời gian.

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

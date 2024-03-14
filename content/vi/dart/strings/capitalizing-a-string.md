---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:09.957909-07:00
description: "Vi\u1EC7c vi\u1EBFt hoa m\u1ED9t chu\u1ED7i bao g\u1ED3m vi\u1EC7c ch\u1EC9\
  nh s\u1EEDa ch\u1EEF c\xE1i \u0111\u1EA7u ti\xEAn c\u1EE7a m\u1ED9t t\u1EEB ho\u1EB7\
  c c\u1EA3 c\xE2u th\xE0nh ch\u1EEF in hoa, trong khi gi\u1EEF nguy\xEAn c\xE1c k\xFD\
  \ t\u1EF1 c\xF2n l\u1EA1i. C\xE1c l\u1EADp\u2026"
lastmod: '2024-03-13T22:44:36.240320-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c vi\u1EBFt hoa m\u1ED9t chu\u1ED7i bao g\u1ED3m vi\u1EC7c ch\u1EC9\
  nh s\u1EEDa ch\u1EEF c\xE1i \u0111\u1EA7u ti\xEAn c\u1EE7a m\u1ED9t t\u1EEB ho\u1EB7\
  c c\u1EA3 c\xE2u th\xE0nh ch\u1EEF in hoa, trong khi gi\u1EEF nguy\xEAn c\xE1c k\xFD\
  \ t\u1EF1 c\xF2n l\u1EA1i. C\xE1c l\u1EADp\u2026"
title: "Vi\u1EBFt hoa m\u1ED9t chu\u1ED7i k\xFD t\u1EF1"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc viết hoa một chuỗi bao gồm việc chỉnh sửa chữ cái đầu tiên của một từ hoặc cả câu thành chữ in hoa, trong khi giữ nguyên các ký tự còn lại. Các lập trình viên thường sử dụng kỹ thuật này trong việc định dạng đầu vào của người dùng hoặc hiển thị văn bản để đảm bảo tính nhất quán hoặc tuân theo các quy tắc ngữ pháp trong giao diện người dùng.

## Làm thế nào:

### Sử dụng Các Phương Thức Của Dart

Dart cung cấp các phương thức đơn giản, dễ hiểu cho việc thao tác chuỗi. Để viết hoa một từ hoặc một câu, bạn thường lấy ký tự đầu tiên, chuyển nó thành chữ in hoa, và sau đó nối nó với phần còn lại của chuỗi. Dưới đây là cách bạn có thể thực hiện:

```dart
String capitalize(String text) {
  if (text.isEmpty) return text;
  return text[0].toUpperCase() + text.substring(1).toLowerCase();
}

void main() {
  var example = "hello world";
  print(capitalize(example)); // Kết quả: Hello world
}
```

### Viết hoa mỗi từ

Để viết hoa chữ cái đầu tiên của mỗi từ trong một chuỗi, bạn có thể tách chuỗi thành các từ, viết hoa mỗi từ, và sau đó nối chúng lại với nhau:

```dart
String capitalizeWords(String text) {
  return text.split(' ').map(capitalize).join(' ');
}

void main() {
  var example = "hello dart enthusiasts";
  print(capitalizeWords(example)); // Kết quả: Hello Dart Enthusiasts
}
```

### Sử dụng Thư viện Bên thứ ba

Mặc dù thư viện tiêu chuẩn của Dart đáp ứng nhu cầu cơ bản, một số nhiệm vụ có thể được thực hiện một cách tiện lợi hơn bằng cách sử dụng các gói bên thứ ba. Một lựa chọn phổ biến cho các khả năng thao tác chuỗi mở rộng, bao gồm việc viết hoa, là gói [`recase`](https://pub.dev/packages/recase). Sau khi thêm nó vào `pubspec.yaml` của dự án, bạn có thể dễ dàng viết hoa các chuỗi cùng các chức năng khác:

```dart
import 'package:recase/recase.dart';

void main() {
  var example = "hello world";
  var rc = ReCase(example);

  print(rc.titleCase); // Kết quả: Hello World
}
```

Sử dụng `recase`, bạn có thể viết hoa từng từ, toàn bộ câu, hoặc thậm chí tuân theo các quy ước chữ viết khác mà không cần xử lý thủ công các biến đổi chuỗi.

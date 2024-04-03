---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:47.391316-07:00
description: "L\xE0m th\u1EBF n\xE0o: Dart s\u1EED d\u1EE5ng th\u01B0 vi\u1EC7n `dart:io`\
  \ \u0111\u1EC3 l\xE0m vi\u1EC7c v\u1EDBi t\u1EC7p v\xE0 th\u01B0 m\u1EE5c. D\u01B0\
  \u1EDBi \u0111\xE2y l\xE0 m\u1ED9t c\xE1ch \u0111\u01A1n gi\u1EA3n \u0111\u1EC3\
  \ ki\u1EC3m tra xem m\u1ED9t th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4ng."
lastmod: '2024-03-13T22:44:36.282324-06:00'
model: gpt-4-0125-preview
summary: "Dart s\u1EED d\u1EE5ng th\u01B0 vi\u1EC7n `dart:io` \u0111\u1EC3 l\xE0m\
  \ vi\u1EC7c v\u1EDBi t\u1EC7p v\xE0 th\u01B0 m\u1EE5c."
title: "Ki\u1EC3m tra xem th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4ng"
weight: 20
---

## Làm thế nào:
Dart sử dụng thư viện `dart:io` để làm việc với tệp và thư mục. Dưới đây là một cách đơn giản để kiểm tra xem một thư mục có tồn tại không:

```dart
import 'dart:io';

void main() {
  var directory = Directory('đường/dẫn/đến/thư/mục/của/bạn');

  if (directory.existsSync()) {
    print('Thư mục tồn tại');
  } else {
    print('Thư mục không tồn tại');
  }
}
```
Kết quả mẫu nếu thư mục tồn tại:
```
Thư mục tồn tại
```

Hoặc, nếu nó không tồn tại:
```
Thư mục không tồn tại
```

Để xử lý các tình huống phức tạp hơn, chẳng hạn như kiểm tra bất đồng bộ hoặc tạo một thư mục nếu nó không tồn tại, bạn có thể sử dụng cách tiếp cận sau:

```dart
import 'dart:io';

void main() async {
  var directory = Directory('đường/dẫn/đến/thư/mục/của/bạn');

  // Kiểm tra bất đồng bộ xem thư mục có tồn tại không
  var exists = await directory.exists();
  if (exists) {
    print('Thư mục tồn tại');
  } else {
    print('Thư mục không tồn tại, đang tạo...');
    await directory.create(); // Lệnh này tạo thư mục
    print('Thư mục đã được tạo');
  }
}
```

Kết quả mẫu nếu thư mục không tồn tại và đã được tạo:
```
Thư mục không tồn tại, đang tạo...
Thư mục đã được tạo
```

Khả năng tích hợp của Dart thường đủ để xử lý tệp và thư mục, vì vậy thư viện từ bên thứ ba thường không cần thiết cho công việc này. Tuy nhiên, đối với các hoạt động hệ thống tệp phức tạp hơn, các gói như `path` (để thao tác với đường dẫn một cách không phụ thuộc vào nền tảng) có thể bổ sung cho thư viện `dart:io` nhưng không trực tiếp cung cấp các kiểm tra tồn tại thư mục tiên tiến hơn so với những gì đã được trình bày.

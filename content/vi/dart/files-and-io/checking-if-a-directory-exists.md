---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:47.391316-07:00
description: "Ki\u1EC3m tra xem m\u1ED9t th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1\
  i trong Dart kh\xF4ng l\xE0 vi\u1EC7c x\xE1c minh s\u1EF1 hi\u1EC7n di\u1EC7n c\u1EE7\
  a m\u1ED9t th\u01B0 m\u1EE5c t\u1EA1i m\u1ED9t \u0111\u01B0\u1EDDng d\u1EABn \u0111\
  \xE3 ch\u1EC9 \u0111\u1ECBnh tr\xEAn h\u1EC7 th\u1ED1ng t\u1EC7p tr\u01B0\u1EDB\
  c\u2026"
lastmod: '2024-03-13T22:44:36.282324-06:00'
model: gpt-4-0125-preview
summary: "Ki\u1EC3m tra xem m\u1ED9t th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i trong\
  \ Dart kh\xF4ng l\xE0 vi\u1EC7c x\xE1c minh s\u1EF1 hi\u1EC7n di\u1EC7n c\u1EE7\
  a m\u1ED9t th\u01B0 m\u1EE5c t\u1EA1i m\u1ED9t \u0111\u01B0\u1EDDng d\u1EABn \u0111\
  \xE3 ch\u1EC9 \u0111\u1ECBnh tr\xEAn h\u1EC7 th\u1ED1ng t\u1EC7p tr\u01B0\u1EDB\
  c khi th\u1EF1c hi\u1EC7n c\xE1c ho\u1EA1t \u0111\u1ED9ng nh\u01B0 \u0111\u1ECD\
  c ho\u1EB7c ghi t\u1EC7p."
title: "Ki\u1EC3m tra xem th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4ng"
weight: 20
---

## Cái gì & Tại sao?

Kiểm tra xem một thư mục có tồn tại trong Dart không là việc xác minh sự hiện diện của một thư mục tại một đường dẫn đã chỉ định trên hệ thống tệp trước khi thực hiện các hoạt động như đọc hoặc ghi tệp. Các lập trình viên làm điều này để tránh các lỗi xảy ra khi cố gắng truy cập hoặc chỉnh sửa các thư mục không tồn tại.

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

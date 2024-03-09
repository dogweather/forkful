---
title:                "Kiểm tra xem thư mục có tồn tại không"
date:                  2024-03-08T21:53:47.391316-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

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

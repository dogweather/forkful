---
title:                "Đọc một tệp văn bản"
date:                  2024-03-08T21:55:58.149277-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Đọc một tệp văn bản trong Dart liên quan đến việc truy cập và lấy dữ liệu từ các tệp lưu trữ trên hệ thống tệp. Các lập trình viên thực hiện điều này để xử lý dữ liệu nhập vào, cài đặt cấu hình, hoặc đọc các bộ dữ liệu, làm cho nó trở thành một hoạt động cơ bản cho nhiều ứng dụng từ các script đơn giản đến các ứng dụng phức tạp.

## Làm thế nào:

Thư viện cốt lõi của Dart, `dart:io`, cung cấp các chức năng cần thiết để đọc tệp văn bản một cách đồng bộ hoặc bất đồng bộ. Dưới đây là cách tiếp cận cả hai.

**Một cách đồng bộ:**

```dart
import 'dart:io';

void main() {
  var tenTep = "path/to/your/textfile.txt";
  var tep = File(tenTep);

  // Đọc tệp một cách đồng bộ
  var noiDung;
  try {
    noiDung = tep.readAsStringSync();
    print(noiDung);
  } catch (e) {
    print('Lỗi khi đọc tệp: $e');
  }
}
```

**Một cách bất đồng bộ:**

Để tránh chặn chương trình trong khi tệp đang được đọc, đặc biệt hữu ích cho các tệp lớn hoặc các ứng dụng nhanh chóng phản hồi:

```dart
import 'dart:io';

void main() async {
  var tenTep = "path/to/your/textfile.txt";
  var tep = File(tenTep);

  try {
    String noiDung = await tep.readAsString();
    print(noiDung);
  } catch (e) {
    print('Lỗi khi đọc tệp: $e');
  }
}
```

**Output Mẫu:**

Nếu tệp văn bản của bạn chứa:

```
Hello, Dart!
```

Cả hai phương pháp trên sẽ đưa ra:

```
Hello, Dart!
```

**Sử dụng Thư viện Bên Thứ Ba:**

Để có thêm các tính năng như thao tác tệp đơn giản hơn hay xử lý lỗi nâng cao, bạn có thể xem xét các thư viện bên thứ ba như `package:file`. Tuy nhiên, tính đến cập nhật cuối cùng của tôi, sử dụng trực tiếp gói `dart:io` cốt lõi, như đã miêu tả ở trên, là phương pháp phổ biến và đơn giản nhất để đọc tệp văn bản trong Dart.

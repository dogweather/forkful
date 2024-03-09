---
title:                "Lấy ngày hiện tại"
date:                  2024-03-08T21:55:01.561400-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Việc lấy ngày hiện tại trong Dart đề cập đến việc truy vấn hệ thống để lấy ngày và giờ hiện tại. Chức năng này thường được sử dụng trong các ứng dụng cho các tính năng như đánh dấu thời gian sự kiện, hiển thị ngày hiện tại cho người dùng, hoặc tính toán thời lượng. Việc biết cách lấy và thao tác với ngày hiện tại một cách hiệu quả là cơ bản cho việc lập lịch, ghi nhật ký, và các tính năng nhạy cảm với thời gian.

## Cách thực hiện:
Thư viện cốt lõi của Dart cung cấp quyền truy cập trực tiếp vào ngày và giờ hiện tại thông qua lớp `DateTime`. Dưới đây là ví dụ cơ bản để lấy ngày hiện tại:

```dart
void main() {
  DateTime now = DateTime.now();
  print(now); // Ví dụ đầu ra: 2023-04-12 10:00:00.000
}
```

Nếu bạn chỉ cần phần ngày (năm, tháng, ngày), bạn có thể định dạng đối tượng `DateTime`:

```dart
void main() {
  DateTime now = DateTime.now();
  String formattedDate = "${now.year}-${now.month}-${now.day}";
  print(formattedDate); // Ví dụ đầu ra: 2023-04-12
}
```

Dart không bao gồm thư viện tích hợp sẵn cho việc định dạng ngày phức tạp, nhưng bạn có thể sử dụng gói `intl` cho mục đích này. Trước tiên, thêm gói vào `pubspec.yaml` của bạn:

```yaml
dependencies:
  intl: ^0.17.0
```

Sau đó, bạn có thể định dạng ngày một cách dễ dàng:

```dart
import 'package:intl/intl.dart';

void main() {
  DateTime now = DateTime.now();
  String formattedDate = DateFormat('yyyy-MM-dd').format(now);
  print(formattedDate); // Ví dụ đầu ra: 2023-04-12
}
```

Để tìm hiểu thêm về các tùy chọn định dạng nâng cao, hãy khám phá lớp `DateFormat` được cung cấp bởi gói `intl`, hỗ trợ một loạt các mẫu và địa phương hóa.

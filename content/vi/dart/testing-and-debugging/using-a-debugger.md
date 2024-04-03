---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:15.288029-07:00
description: '#'
lastmod: '2024-03-13T22:44:36.269076-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "S\u1EED d\u1EE5ng m\u1ED9t c\xF4ng c\u1EE5 g\u1EE1 l\u1ED7i"
weight: 35
---

## Làm thế nào:


### Gỡ lỗi Cơ bản:
**1. Đặt điểm dừng:**

Để đặt một điểm dừng, chỉ cần nhấp vào lề bên trái của dòng code trong IDE của bạn (ví dụ, Visual Studio Code hoặc Android Studio) nơi bạn muốn thực thi tạm dừng.

```dart
void main() {
  var message = 'Hello, Debugging';
  print(message); // Đặt một điểm dừng ở đây
}
```

**2. Bắt đầu Gỡ lỗi:**

Trong IDE của bạn, bắt đầu một phiên gỡ lỗi bằng cách nhấp vào biểu tượng gỡ lỗi hoặc nhấn nút gỡ lỗi. Thực thi sẽ tạm dừng tại các điểm dừng.

**3. Kiểm tra Biến:**

Khi thực thi bị tạm dừng, di chuột qua các biến để xem giá trị hiện tại của chúng.

**4. Theo dõi từng Bước qua Code:**

Sử dụng các lệnh bước qua, bước vào, và bước ra trong IDE của bạn để điều hướng qua code của bạn từng dòng hoặc hàm một.

### Gỡ lỗi Nâng cao với Observatory:
Dart bao gồm một công cụ gọi là Observatory cho việc gỡ lỗi và phân tích hiệu suất các ứng dụng Dart. Nó đặc biệt hữu ích cho các ứng dụng chạy trên Dart VM.

**Truy cập Observatory:**

Chạy ứng dụng Dart của bạn với cờ `--observe`.

```bash
dart --observe your_program.dart
```

Lệnh này in một URL ra console, mà bạn có thể mở trong trình duyệt web để truy cập vào debugger Observatory.

### Sử dụng Thư viện của Bên thứ ba Phổ biến:
Để gỡ lỗi các ứng dụng Flutter, gói `flutter_devtools` cung cấp một bộ công cụ hiệu suất và gỡ lỗi tích hợp với cả Dart VM và Flutter.

**Cài đặt:**

Đầu tiên, thêm `devtools` vào tệp `pubspec.yaml` của bạn dưới `dev_dependencies`:

```yaml
dev_dependencies:
  devtools: any
```

**Khởi chạy DevTools:**

Chạy lệnh này trong terminal của bạn:

```bash
flutter pub global run devtools
```

Sau đó, bắt đầu ứng dụng Flutter của bạn trong chế độ debug. DevTools cung cấp các tính năng như Flutter inspector cho phân tích cây widget, và mạng profiler để giám sát hoạt động mạng.

### Mẫu Đầu ra:
Khi gặp một điểm dừng, IDE của bạn có thể hiển thị giá trị biến và truy vết ngăn xếp như sau:

```
message: 'Hello, Debugging'
```

Bằng cách sử dụng hiệu quả các công cụ và kỹ thuật gỡ lỗi trong Dart, các nhà phát triển có thể xác định và giải quyết vấn đề nhanh chóng hơn, dẫn đến quá trình phát triển suôn sẻ hơn và các ứng dụng mạnh mẽ hơn.

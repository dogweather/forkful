---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:37.652744-07:00
description: "C\xE1ch l\xE0m: Trong Dart, b\u1EA1n c\xF3 th\u1EC3 s\u1EED d\u1EE5\
  ng g\xF3i `http` \u0111\u1EC3 g\u1EEDi c\xE1c y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1\
  c th\u1EF1c c\u01A1 b\u1EA3n. \u0110\u1EA7u ti\xEAn, th\xEAm g\xF3i `http` v\xE0\
  o t\u1EADp tin `pubspec.yaml` c\u1EE7a\u2026"
lastmod: '2024-03-13T22:44:36.262504-06:00'
model: gpt-4-0125-preview
summary: "Trong Dart, b\u1EA1n c\xF3 th\u1EC3 s\u1EED d\u1EE5ng g\xF3i `http` \u0111\
  \u1EC3 g\u1EEDi c\xE1c y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1 b\u1EA3\
  n."
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1 b\u1EA3\
  n"
weight: 45
---

## Cách làm:
Trong Dart, bạn có thể sử dụng gói `http` để gửi các yêu cầu HTTP với xác thực cơ bản. Đầu tiên, thêm gói `http` vào tập tin `pubspec.yaml` của bạn:

```yaml
dependencies:
  http: ^0.13.4
```

Sau đó, nhập gói vào tập tin Dart của bạn:

```dart
import 'package:http/http.dart' as http;
import 'dart:convert';
```

Để gửi một yêu cầu GET với xác thực cơ bản, bạn có thể sử dụng mã sau:

```dart
Future<void> fetchUserData() async {
  final username = 'yourUsername';
  final password = 'yourPassword';
  final credentials = base64Encode(utf8.encode('$username:$password'));
  final response = await http.get(
    Uri.parse('https://yourapi.com/userdata'),
    headers: {
      'Authorization': 'Basic $credentials',
    },
  );

  if (response.statusCode == 200) {
    print('Lấy dữ liệu người dùng thành công!');
    print('Nội dung phản hồi: ${response.body}');
  } else {
    print('Không thể lấy dữ liệu người dùng với mã trạng thái: ${response.statusCode}');
  }
}
```

Mã này gửi một yêu cầu GET đến 'https://yourapi.com/userdata' với một tiêu đề xác thực cơ bản. Tên đăng nhập và mật khẩu được mã hóa dạng base64 và được truyền trong tiêu đề 'Authorization' theo chuẩn xác thực truy cập cơ bản.

**Mẫu kết quả:**

Khi yêu cầu thành công và nếu máy chủ trả về một mã trạng thái 200, bạn có thể thấy:

```plaintext
Lấy dữ liệu người dùng thành công!
Nội dung phản hồi: {"id":1, "name":"John Doe", "email":"john@example.com"}
```

Nếu xác thực không thành công hoặc có bất kỳ lỗi nào khác, mã trạng thái phản hồi sẽ giúp xác định vấn đề.

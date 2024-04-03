---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:32.845750-07:00
description: "L\xE0m th\u1EBF n\xE0o: Dart cung c\u1EA5p g\xF3i `http`, m\u1ED9t th\u01B0\
  \ vi\u1EC7n b\xEAn th\u1EE9 ba ph\u1ED5 bi\u1EBFn \u0111\u1EC3 th\u1EF1c hi\u1EC7\
  n c\xE1c y\xEAu c\u1EA7u HTTP. D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t v\xED d\u1EE5\
  \ c\u01A1 b\u1EA3n v\u1EC1 c\xE1ch s\u1EED d\u1EE5ng n\xF3 \u0111\u1EC3\u2026"
lastmod: '2024-03-13T22:44:36.261172-06:00'
model: gpt-4-0125-preview
summary: "Dart cung c\u1EA5p g\xF3i `http`, m\u1ED9t th\u01B0 vi\u1EC7n b\xEAn th\u1EE9\
  \ ba ph\u1ED5 bi\u1EBFn \u0111\u1EC3 th\u1EF1c hi\u1EC7n c\xE1c y\xEAu c\u1EA7u\
  \ HTTP."
title: "T\u1EA3i trang web v\u1EC1 m\xE1y"
weight: 42
---

## Làm thế nào:
Dart cung cấp gói `http`, một thư viện bên thứ ba phổ biến để thực hiện các yêu cầu HTTP. Dưới đây là một ví dụ cơ bản về cách sử dụng nó để tải một trang web:

Trước tiên, thêm gói `http` vào `pubspec.yaml` của bạn:

```yaml
dependencies:
  http: ^0.13.3
```

Sau đó, nhập gói và sử dụng nó để lấy nội dung của một trang web:

```dart
import 'package:http/http.dart' as http;

Future<void> main() async {
  var url = Uri.parse('http://example.com');
  var response = await http.get(url);
  if (response.statusCode == 200) {
    print('Trang đã được tải xuống:');
    print(response.body);
  } else {
    print('Yêu cầu thất bại với trạng thái: ${response.statusCode}.');
  }
}
```

**Đầu ra mẫu** (điều này sẽ thay đổi dựa trên nội dung của trang web):

```
Trang đã được tải xuống:
<!doctype html>
<html>
<head>
    <title>Ví dụ Tên Miền</title>
...
</html>
```

Đối với những tình huống phức tạp hơn, như xử lý cookies hoặc thiết lập tiêu đề user-agent, bạn sẽ sử dụng cùng một gói `http` nhưng với cấu hình bổ sung cho yêu cầu của mình:

```dart
import 'package:http/http.dart' as http;

Future<void> main() async {
  var headers = {
    'User-Agent': 'YourCustomUserAgent/1.0',
    'Cookie': 'name=value; name2=value2',
  };
  var url = Uri.parse('http://example.com');
  var response = await http.get(url, headers: headers);

  if (response.statusCode == 200) {
    print('Trang được tải xuống với tiêu đề tuỳ chỉnh:');
    print(response.body);
  } else {
    print('Yêu cầu thất bại với trạng thái: ${response.statusCode}.');
  }
}
```

Sử dụng các tiêu đề như thế này có thể mô phỏng chính xác hơn các yêu cầu trình duyệt, đặc biệt hữu ích khi đối mặt với các trang web có yêu cầu cụ thể hoặc bảo vệ chống lại việc lấy dữ liệu.

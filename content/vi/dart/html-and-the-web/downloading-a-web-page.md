---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:32.845750-07:00
description: "T\u1EA3i m\u1ED9t trang web xu\u1ED1ng bao g\u1ED3m vi\u1EC7c l\u1EA5\
  y n\u1ED9i dung c\u1EE7a trang web th\xF4ng qua URL c\u1EE7a n\xF3 \u0111\u1EC3\
  \ x\u1EED l\xFD ho\u1EB7c l\u01B0u tr\u1EEF. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0\
  m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 tr\xEDch xu\u1EA5t\u2026"
lastmod: '2024-03-09T21:06:01.002573-07:00'
model: gpt-4-0125-preview
summary: "T\u1EA3i m\u1ED9t trang web xu\u1ED1ng bao g\u1ED3m vi\u1EC7c l\u1EA5y n\u1ED9\
  i dung c\u1EE7a trang web th\xF4ng qua URL c\u1EE7a n\xF3 \u0111\u1EC3 x\u1EED l\xFD\
  \ ho\u1EB7c l\u01B0u tr\u1EEF. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1\
  u n\xE0y \u0111\u1EC3 tr\xEDch xu\u1EA5t\u2026"
title: "T\u1EA3i trang web v\u1EC1 m\xE1y"
---

{{< edit_this_page >}}

## Gì & Tại sao?

Tải một trang web xuống bao gồm việc lấy nội dung của trang web thông qua URL của nó để xử lý hoặc lưu trữ. Các lập trình viên làm điều này để trích xuất thông tin, theo dõi các thay đổi, hoặc lưu trữ nội dung, làm cho nó trở thành một phần cơ bản trong việc lấy dữ liệu trang web, khai thác dữ liệu, và nhiệm vụ kiểm tra tự động.

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

---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:24.367492-07:00
description: "Vi\u1EC7c g\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP trong Dart l\xE0 quy\
  \ tr\xECnh kh\u1EDFi t\u1EA1o giao ti\u1EBFp v\u1EDBi m\xE1y ch\u1EE7 web ho\u1EB7\
  c API t\u1EEB m\u1ED9t \u1EE9ng d\u1EE5ng Dart. L\u1EADp tr\xECnh vi\xEAn th\u1EF1\
  c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3\u2026"
lastmod: '2024-03-13T22:44:36.258592-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c g\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP trong Dart l\xE0 quy tr\xEC\
  nh kh\u1EDFi t\u1EA1o giao ti\u1EBFp v\u1EDBi m\xE1y ch\u1EE7 web ho\u1EB7c API\
  \ t\u1EEB m\u1ED9t \u1EE9ng d\u1EE5ng Dart."
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP"
weight: 44
---

## Cách thực hiện:
Dart bao gồm gói `http`, một cách mạnh mẽ và thuận tiện để làm việc với các tài nguyên HTTP. Đầu tiên, bao gồm nó trong tệp pubspec.yaml của bạn:

```yaml
dependencies:
  http: ^0.13.3
```

Sau đó, nhập nó vào code Dart của bạn để bắt đầu thực hiện các yêu cầu:

```dart
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/todos/1');
  var phản_hồi = await http.get(url);

  if (phản_hồi.statusCode == 200) {
    print('Nội dung phản hồi: ${phản_hồi.body}');
  } else {
    print('Yêu cầu thất bại với trạng thái: ${phản_hồi.statusCode}.');
  }
}
```

Mẫu đầu ra cho một yêu cầu thành công có thể trông như thế này:

```
Nội dung phản hồi: {
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

Đối với các yêu cầu phức tạp hơn, như yêu cầu POST với một nội dung JSON, bạn sẽ thực hiện như sau:

```dart
import 'dart:convert';
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/posts');
  var phản_hồi = await http.post(
    url,
    headers: {"Content-Type": "application/json"},
    body: jsonEncode({
      "title": 'foo',
      "body": 'bar',
      "userId": 1,
    }),
  );

  if (phản_hồi.statusCode == 201) {
    print('Trạng thái phản hồi: ${phản_hồi.statusCode}');
    print('Nội dung phản hồi: ${phản_hồi.body}');
  } else {
    print('Thất bại khi tạo một bài đăng mới. Trạng thái: ${phản_hồi.statusCode}');
  }
}
```

Mẫu đầu ra cho yêu cầu post có thể là:

```
Trạng thái phản hồi: 201
Nội dung phản hồi: {
  "title": "foo",
  "body": "bar",
  "userId": 1,
  "id": 101
}
```

Những ví dụ này trình bày các yêu cầu HTTP GET và POST cơ bản sử dụng gói `http` trong Dart. Gói này đáp ứng hầu hết các nhu cầu về gửi yêu cầu HTTP, bao gồm cả những tình huống phức tạp hơn với headers và nội dung body.

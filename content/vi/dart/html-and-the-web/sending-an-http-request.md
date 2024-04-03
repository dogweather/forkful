---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:24.367492-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Dart bao g\u1ED3m g\xF3i `http`, m\u1ED9\
  t c\xE1ch m\u1EA1nh m\u1EBD v\xE0 thu\u1EADn ti\u1EC7n \u0111\u1EC3 l\xE0m vi\u1EC7\
  c v\u1EDBi c\xE1c t\xE0i nguy\xEAn HTTP. \u0110\u1EA7u ti\xEAn, bao g\u1ED3m n\xF3\
  \ trong t\u1EC7p pubspec.yaml\u2026"
lastmod: '2024-03-13T22:44:36.258592-06:00'
model: gpt-4-0125-preview
summary: "Dart bao g\u1ED3m g\xF3i `http`, m\u1ED9t c\xE1ch m\u1EA1nh m\u1EBD v\xE0\
  \ thu\u1EADn ti\u1EC7n \u0111\u1EC3 l\xE0m vi\u1EC7c v\u1EDBi c\xE1c t\xE0i nguy\xEA\
  n HTTP."
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

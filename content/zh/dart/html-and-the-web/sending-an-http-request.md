---
title:                "发送HTTP请求"
date:                  2024-03-08T21:56:16.373682-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 Dart 中发送 HTTP 请求是指从 Dart 应用程序启动与网页服务器或 API 的通信的过程。程序员这么做是为了从网络获取数据、提交表单以及与 RESTful 服务进行交互，这使其成为了 Dart 中 web、服务器端和移动应用开发的基本操作。

## 如何操作：

Dart 包括 `http` 包，这是一个强大且方便的方式来处理 HTTP 资源。首先，在你的 pubspec.yaml 文件中包含它：

```yaml
dependencies:
  http: ^0.13.3
```

然后，在你的 Dart 代码中导入它，开始发起请求：

```dart
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/todos/1');
  var response = await http.get(url);

  if (response.statusCode == 200) {
    print('响应体：${response.body}');
  } else {
    print('请求失败，状态码为：${response.statusCode}。');
  }
}
```

成功请求的示例输出可能如下所示：

```
响应体：{
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

对于更复杂的请求，比如带有 JSON 主体的 POST 请求，你可以这样做：

```dart
import 'dart:convert';
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/posts');
  var response = await http.post(
    url,
    headers: {"Content-Type": "application/json"},
    body: jsonEncode({
      "title": 'foo',
      "body": 'bar',
      "userId": 1,
    }),
  );

  if (response.statusCode == 201) {
    print('响应状态：${response.statusCode}');
    print('响应体：${response.body}');
  } else {
    print('创建新帖子失败。状态：${response.statusCode}');
  }
}
```

POST 请求的示例输出可能是：

```
响应状态：201
响应体：{
  "title": "foo",
  "body": "bar",
  "userId": 1,
  "id": 101
}
```

这些示例展示了使用 Dart 中的 `http` 包发出基本 HTTP GET 和 POST 请求。此包涵盖了发送 HTTP 请求的大多数需求，包括带有头部和主体内容的更复杂场景。

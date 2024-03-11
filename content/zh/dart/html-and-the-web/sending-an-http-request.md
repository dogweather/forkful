---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:16.373682-07:00
description: "\u5728 Dart \u4E2D\u53D1\u9001 HTTP \u8BF7\u6C42\u662F\u6307\u4ECE Dart\
  \ \u5E94\u7528\u7A0B\u5E8F\u542F\u52A8\u4E0E\u7F51\u9875\u670D\u52A1\u5668\u6216\
  \ API \u7684\u901A\u4FE1\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\
  \u662F\u4E3A\u4E86\u4ECE\u7F51\u7EDC\u83B7\u53D6\u6570\u636E\u3001\u63D0\u4EA4\u8868\
  \u5355\u4EE5\u53CA\u4E0E RESTful \u670D\u52A1\u8FDB\u884C\u4EA4\u4E92\uFF0C\u8FD9\
  \u4F7F\u5176\u6210\u4E3A\u4E86 Dart \u4E2D web\u3001\u670D\u52A1\u5668\u7AEF\u548C\
  \u79FB\u52A8\u5E94\u7528\u5F00\u53D1\u7684\u57FA\u672C\u64CD\u4F5C\u3002"
lastmod: '2024-03-11T00:14:21.172190-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Dart \u4E2D\u53D1\u9001 HTTP \u8BF7\u6C42\u662F\u6307\u4ECE Dart\
  \ \u5E94\u7528\u7A0B\u5E8F\u542F\u52A8\u4E0E\u7F51\u9875\u670D\u52A1\u5668\u6216\
  \ API \u7684\u901A\u4FE1\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\
  \u662F\u4E3A\u4E86\u4ECE\u7F51\u7EDC\u83B7\u53D6\u6570\u636E\u3001\u63D0\u4EA4\u8868\
  \u5355\u4EE5\u53CA\u4E0E RESTful \u670D\u52A1\u8FDB\u884C\u4EA4\u4E92\uFF0C\u8FD9\
  \u4F7F\u5176\u6210\u4E3A\u4E86 Dart \u4E2D web\u3001\u670D\u52A1\u5668\u7AEF\u548C\
  \u79FB\u52A8\u5E94\u7528\u5F00\u53D1\u7684\u57FA\u672C\u64CD\u4F5C\u3002"
title: "\u53D1\u9001HTTP\u8BF7\u6C42"
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

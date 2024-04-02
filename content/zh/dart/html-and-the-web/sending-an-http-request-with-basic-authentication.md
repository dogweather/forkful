---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:40.852072-07:00
description: "\u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\
  \u6D89\u53CA\u5728\u8BF7\u6C42\u4E2D\u9644\u52A0\u7528\u6237\u540D\u548C\u5BC6\u7801\
  \u4EE5\u9A8C\u8BC1\u7528\u6237\u8EAB\u4EFD\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\u5B83\
  \u6765\u8BBF\u95EE\u9700\u8981\u8BA4\u8BC1\u7684\u8D44\u6E90\uFF0C\u786E\u4FDD\u5BA2\
  \u6237\u7AEF\u4E0E\u670D\u52A1\u5668\u4E4B\u95F4\u7684\u5B89\u5168\u901A\u4FE1\u3002"
lastmod: '2024-03-13T22:44:47.417615-06:00'
model: gpt-4-0125-preview
summary: "\u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\u6D89\
  \u53CA\u5728\u8BF7\u6C42\u4E2D\u9644\u52A0\u7528\u6237\u540D\u548C\u5BC6\u7801\u4EE5\
  \u9A8C\u8BC1\u7528\u6237\u8EAB\u4EFD\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\u5B83\u6765\
  \u8BBF\u95EE\u9700\u8981\u8BA4\u8BC1\u7684\u8D44\u6E90\uFF0C\u786E\u4FDD\u5BA2\u6237\
  \u7AEF\u4E0E\u670D\u52A1\u5668\u4E4B\u95F4\u7684\u5B89\u5168\u901A\u4FE1\u3002"
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001HTTP\u8BF7\u6C42"
weight: 45
---

## 什么与为什么？

发送带有基本认证的HTTP请求涉及在请求中附加用户名和密码以验证用户身份。程序员使用它来访问需要认证的资源，确保客户端与服务器之间的安全通信。

## 如何操作：

在Dart中，你可以使用`http`软件包发送带有基本认证的HTTP请求。首先，在你的`pubspec.yaml`文件中添加`http`包：

```yaml
dependencies:
  http: ^0.13.4
```

然后，在你的Dart文件中导入包：

```dart
import 'package:http/http.dart' as http;
import 'dart:convert';
```

要发送带有基本认证的GET请求，你可以使用以下代码：

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
    print('成功获取用户数据！');
    print('响应体：${response.body}');
  } else {
    print('获取用户数据失败，状态码为：${response.statusCode}');
  }
}
```

此代码向'https://yourapi.com/userdata'发送GET请求，并带有基本认证头。用户名和密码被编码为base64，并按照基本访问认证标准通过'Authorization'头传递。

**样本输出：**

请求成功并且如果服务器返回状态码为200，你可能会看到：

```plaintext
成功获取用户数据！
响应体：{"id":1, "name":"John Doe", "email":"john@example.com"}
```

如果认证失败或存在任何其他错误，响应状态码将帮助识别问题。

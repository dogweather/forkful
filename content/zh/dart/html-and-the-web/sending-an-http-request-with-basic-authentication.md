---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:40.852072-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728Dart\u4E2D\uFF0C\u4F60\u53EF\u4EE5\
  \u4F7F\u7528`http`\u8F6F\u4EF6\u5305\u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\
  \u7684HTTP\u8BF7\u6C42\u3002\u9996\u5148\uFF0C\u5728\u4F60\u7684`pubspec.yaml`\u6587\
  \u4EF6\u4E2D\u6DFB\u52A0`http`\u5305\uFF1A."
lastmod: '2024-04-05T21:53:47.747621-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001HTTP\u8BF7\u6C42"
weight: 45
---

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

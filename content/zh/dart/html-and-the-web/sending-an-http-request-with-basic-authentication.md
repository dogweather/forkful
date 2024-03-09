---
title:                "使用基本认证发送HTTP请求"
date:                  2024-03-08T21:56:40.852072-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

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

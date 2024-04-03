---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:42.277939-07:00
description: "\u5982\u4F55\u64CD\u4F5C: Dart\u63D0\u4F9B\u4E86`http`\u5305\uFF0C\u8FD9\
  \u662F\u4E00\u4E2A\u7528\u4E8E\u8FDB\u884CHTTP\u8BF7\u6C42\u7684\u6D41\u884C\u7684\
  \u7B2C\u4E09\u65B9\u5E93\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\u4F7F\u7528\u5B83\u6765\
  \u4E0B\u8F7D\u7F51\u9875\u7684\u57FA\u672C\u793A\u4F8B\uFF1A \u9996\u5148\uFF0C\u5C06\
  `http`\u5305\u6DFB\u52A0\u5230\u4F60\u7684`pubspec.yaml`\u4E2D\uFF1A."
lastmod: '2024-03-13T22:44:47.416475-06:00'
model: gpt-4-0125-preview
summary: "Dart\u63D0\u4F9B\u4E86`http`\u5305\uFF0C\u8FD9\u662F\u4E00\u4E2A\u7528\u4E8E\
  \u8FDB\u884CHTTP\u8BF7\u6C42\u7684\u6D41\u884C\u7684\u7B2C\u4E09\u65B9\u5E93\u3002\
  \u8FD9\u91CC\u6709\u4E00\u4E2A\u4F7F\u7528\u5B83\u6765\u4E0B\u8F7D\u7F51\u9875\u7684\
  \u57FA\u672C\u793A\u4F8B\uFF1A\n\n\u9996\u5148\uFF0C\u5C06`http`\u5305\u6DFB\u52A0\
  \u5230\u4F60\u7684`pubspec.yaml`\u4E2D\uFF1A."
title: "\u4E0B\u8F7D\u7F51\u9875"
weight: 42
---

## 如何操作:
Dart提供了`http`包，这是一个用于进行HTTP请求的流行的第三方库。这里有一个使用它来下载网页的基本示例：

首先，将`http`包添加到你的`pubspec.yaml`中：

```yaml
dependencies:
  http: ^0.13.3
```

然后，导入包并使用它来获取网页的内容：

```dart
import 'package:http/http.dart' as http;

Future<void> main() async {
  var url = Uri.parse('http://example.com');
  var response = await http.get(url);
  if (response.statusCode == 200) {
    print('页面已下载：');
    print(response.body);
  } else {
    print('请求失败，状态码：${response.statusCode}。');
  }
}
```

**样例输出**（这将基于网页内容而有所不同）：

```
页面已下载：
<!doctype html>
<html>
<head>
    <title>示例域名</title>
...
</html>
```

对于更复杂的场景，如处理cookies或设置用户代理头，你会使用相同的`http`包，但对你的请求进行额外配置：

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
    print('页面已下载，使用自定义头：');
    print(response.body);
  } else {
    print('请求失败，状态码：${response.statusCode}。');
  }
}
```

使用这样的头信息可以更准确地模拟浏览器请求，这在处理有特定要求或防止抓取保护的网站时特别有用。

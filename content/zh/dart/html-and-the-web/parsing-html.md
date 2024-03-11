---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:10.706479-07:00
description: "\u5728\u7F16\u7A0B\u4E2D\u89E3\u6790HTML\u6D89\u53CA\u4ECEHTML\u6587\
  \u6863\u4E2D\u63D0\u53D6\u6570\u636E\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\
  \u4E3A\u4E86\u4E0E\u7F51\u9875\u5185\u5BB9\u4E92\u52A8\u6216\u4E3A\u4E86\u4FE1\u606F\
  \u63D0\u53D6\u3001\u6D4B\u8BD5\u6216\u81EA\u52A8\u5316\u76EE\u7684\u6293\u53D6\u7F51\
  \u9875\u5185\u5BB9\uFF0C\u5373\u4F7F\u6CA1\u6709\u5B98\u65B9API\u53EF\u7528\u3002"
lastmod: '2024-03-11T00:14:21.173436-06:00'
model: gpt-4-0125-preview
summary: "\u5728\u7F16\u7A0B\u4E2D\u89E3\u6790HTML\u6D89\u53CA\u4ECEHTML\u6587\u6863\
  \u4E2D\u63D0\u53D6\u6570\u636E\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\
  \u4E86\u4E0E\u7F51\u9875\u5185\u5BB9\u4E92\u52A8\u6216\u4E3A\u4E86\u4FE1\u606F\u63D0\
  \u53D6\u3001\u6D4B\u8BD5\u6216\u81EA\u52A8\u5316\u76EE\u7684\u6293\u53D6\u7F51\u9875\
  \u5185\u5BB9\uFF0C\u5373\u4F7F\u6CA1\u6709\u5B98\u65B9API\u53EF\u7528\u3002"
title: "\u89E3\u6790HTML"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在编程中解析HTML涉及从HTML文档中提取数据。程序员这样做是为了与网页内容互动或为了信息提取、测试或自动化目的抓取网页内容，即使没有官方API可用。

## 如何操作：
Dart在其核心库中不提供对HTML解析的内置支持。然而，你可以使用第三方包如`html`来解析和操作HTML文档。

首先，将`html`包添加到你的`pubspec.yaml`文件中：

```yaml
dependencies:
  html: ^0.15.0
```

然后，将包导入到你的Dart文件中：

```dart
import 'package:html/parser.dart' show parse;
import 'package:html/dom.dart';
```

这里有一个解析包含HTML的字符串并提取数据的基本示例：

```dart
void main() {
  var htmlDocument = """
  <html>
    <body>
      <h1>你好, Dart!</h1>
      <p>这是一个示例HTML中的段落</p>
    </body>
  </html>
  """;

  // 解析HTML字符串
  Document document = parse(htmlDocument);

  // 提取数据
  String title = document.querySelector('h1')?.text ?? "未找到标题";
  String paragraph = document.querySelector('p')?.text ?? "未找到段落";

  print('标题: $title');
  print('段落: $paragraph');
}
```

输出：

```
标题: 你好, Dart!
段落: 这是一个示例HTML中的段落
```

为了与现实世界的网页互动，你可能会将`html`解析与HTTP请求相结合（使用`http`包来获取网页内容）。这里有一个快速示例：

首先，添加`http`包以及`html`：

```yaml
dependencies:
  html: ^0.15.0
  http: ^0.13.3
```

然后，从网上抓取并解析一个HTML页面：

```dart
import 'package:http/http.dart' as http;
import 'package:html/parser.dart' show parse;

void main() async {
  var url = 'https://example.com';
  
  // 获取网页
  var response = await http.get(Uri.parse(url));
  
  if (response.statusCode == 200) {
    var document = parse(response.body);

    // 假设页面有你感兴趣的<h1>标签
    var headlines = document.querySelectorAll('h1').map((e) => e.text).toList();
    
    print('头条: $headlines');
  } else {
    print('请求失败，状态码: ${response.statusCode}.');
  }
}
```

注意：上述展示的网页抓取技术应负责任地使用，并遵守网站的服务条款。

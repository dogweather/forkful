---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:10.706479-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Dart\u5728\u5176\u6838\u5FC3\u5E93\u4E2D\
  \u4E0D\u63D0\u4F9B\u5BF9HTML\u89E3\u6790\u7684\u5185\u7F6E\u652F\u6301\u3002\u7136\
  \u800C\uFF0C\u4F60\u53EF\u4EE5\u4F7F\u7528\u7B2C\u4E09\u65B9\u5305\u5982`html`\u6765\
  \u89E3\u6790\u548C\u64CD\u4F5CHTML\u6587\u6863\u3002 \u9996\u5148\uFF0C\u5C06`html`\u5305\
  \u6DFB\u52A0\u5230\u4F60\u7684`pubspec.yaml`\u6587\u4EF6\u4E2D\uFF1A."
lastmod: '2024-04-05T22:38:46.578154-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A Dart\u5728\u5176\u6838\u5FC3\u5E93\u4E2D\u4E0D\
  \u63D0\u4F9B\u5BF9HTML\u89E3\u6790\u7684\u5185\u7F6E\u652F\u6301\u3002\u7136\u800C\
  \uFF0C\u4F60\u53EF\u4EE5\u4F7F\u7528\u7B2C\u4E09\u65B9\u5305\u5982`html`\u6765\u89E3\
  \u6790\u548C\u64CD\u4F5CHTML\u6587\u6863\u3002 \u9996\u5148\uFF0C\u5C06`html`\u5305\
  \u6DFB\u52A0\u5230\u4F60\u7684`pubspec.yaml`\u6587\u4EF6\u4E2D\uFF1A."
title: "\u89E3\u6790HTML"
weight: 43
---

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

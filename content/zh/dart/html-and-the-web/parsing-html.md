---
title:                "解析HTML"
date:                  2024-03-08T21:56:10.706479-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
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

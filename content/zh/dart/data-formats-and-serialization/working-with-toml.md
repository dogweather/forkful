---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:19.854939-07:00
description: "\u5982\u4F55\u64CD\u4F5C: Dart\u5E76\u4E0D\u5185\u7F6E\u652F\u6301TOML\uFF0C\
  \u4F46\u4F60\u53EF\u4EE5\u4F7F\u7528\u7B2C\u4E09\u65B9\u5305\u5982`toml`\u6765\u5904\
  \u7406TOML\u6587\u4EF6\u3002\u9996\u5148\uFF0C\u5C06`toml`\u6DFB\u52A0\u5230\u4F60\
  \u7684`pubspec.yaml`\u4E2D\uFF1A."
lastmod: '2024-03-13T22:44:47.449853-06:00'
model: gpt-4-0125-preview
summary: "Dart\u5E76\u4E0D\u5185\u7F6E\u652F\u6301TOML\uFF0C\u4F46\u4F60\u53EF\u4EE5\
  \u4F7F\u7528\u7B2C\u4E09\u65B9\u5305\u5982`toml`\u6765\u5904\u7406TOML\u6587\u4EF6\
  \u3002\u9996\u5148\uFF0C\u5C06`toml`\u6DFB\u52A0\u5230\u4F60\u7684`pubspec.yaml`\u4E2D\
  \uFF1A."
title: "\u4F7F\u7528TOML\u5DE5\u4F5C"
weight: 39
---

## 如何操作:
Dart并不内置支持TOML，但你可以使用第三方包如`toml`来处理TOML文件。首先，将`toml`添加到你的`pubspec.yaml`中：

```yaml
dependencies:
  toml: ^0.10.0
```

### 读取TOML
假设你有一个简单的配置文件`config.toml`来读取TOML文件：

```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

你可以如下在Dart中解析这个TOML文件：

```dart
import 'dart:io';
import 'package:toml/toml.dart';

void main() async {
  var content = await File('config.toml').readAsString();
  var doc = TomlDocument.parse(content);
  var data = doc.toMap();

  print(data['database']); // 打印'database'部分
}
```

这会打印：

```dart
{server: 192.168.1.1, ports: [8001, 8001, 8002], connection_max: 5000, enabled: true}
```

### 编写TOML
要创建TOML内容，使用`toml`包提供的`TomlBuilder`：

```dart
import 'package:toml/toml.dart';

void main() {
  final builder = TomlBuilder();

  builder.table('database')
    ..set('server', '192.168.1.1')
    ..set('ports', [8001, 8001, 8002])
    ..set('connection_max', 5000)
    ..set('enabled', true);

  var tomlString = builder.build().toString();
  print(tomlString);
}
```

这将生成并打印一个与我们的`config.toml`文件非常相似的TOML内容字符串：

```toml
[database]
server = "192.168.1.1"
ports = [8001, 8001, 8002]
connection_max = 5000
enabled = true
```

这些示例展示了如何从TOML文件中读取和写入，让你在Dart应用中处理配置数据变得简单。

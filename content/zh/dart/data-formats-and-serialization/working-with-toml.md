---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:19.854939-07:00
description: "TOML\uFF0C\u6216\u79F0\u4E3ATom's Obvious, Minimal Language\uFF08\u6C64\
  \u59C6\u7684\u660E\u663E\u3001\u7B80\u7EA6\u8BED\u8A00\uFF09\uFF0C\u662F\u4E00\u79CD\
  \u914D\u7F6E\u6587\u4EF6\u683C\u5F0F\uFF0C\u7531\u4E8E\u5176\u6E05\u6670\u7684\u8BED\
  \u4E49\u800C\u6613\u4E8E\u9605\u8BFB\u3002\u7A0B\u5E8F\u5458\u4E4B\u6240\u4EE5\u4F7F\
  \u7528\u5B83\u6765\u914D\u7F6E\u8F6F\u4EF6\u5E94\u7528\uFF0C\u662F\u56E0\u4E3A\u5B83\
  \u7B80\u5355\u6613\u4E8E\u89E3\u6790\uFF0C\u5E76\u4E14\u80FD\u591F\u6700\u5C0F\u5316\
  \u6DF7\u6DC6\u6216\u9519\u8BEF\u3002"
lastmod: '2024-03-13T22:44:47.449853-06:00'
model: gpt-4-0125-preview
summary: "TOML\uFF0C\u6216\u79F0\u4E3ATom's Obvious, Minimal Language\uFF08\u6C64\u59C6\
  \u7684\u660E\u663E\u3001\u7B80\u7EA6\u8BED\u8A00\uFF09\uFF0C\u662F\u4E00\u79CD\u914D\
  \u7F6E\u6587\u4EF6\u683C\u5F0F\uFF0C\u7531\u4E8E\u5176\u6E05\u6670\u7684\u8BED\u4E49\
  \u800C\u6613\u4E8E\u9605\u8BFB\u3002\u7A0B\u5E8F\u5458\u4E4B\u6240\u4EE5\u4F7F\u7528\
  \u5B83\u6765\u914D\u7F6E\u8F6F\u4EF6\u5E94\u7528\uFF0C\u662F\u56E0\u4E3A\u5B83\u7B80\
  \u5355\u6613\u4E8E\u89E3\u6790\uFF0C\u5E76\u4E14\u80FD\u591F\u6700\u5C0F\u5316\u6DF7\
  \u6DC6\u6216\u9519\u8BEF\u3002"
title: "\u4F7F\u7528TOML\u5DE5\u4F5C"
---

{{< edit_this_page >}}

## 什么 & 为什么?

TOML，或称为Tom's Obvious, Minimal Language（汤姆的明显、简约语言），是一种配置文件格式，由于其清晰的语义而易于阅读。程序员之所以使用它来配置软件应用，是因为它简单易于解析，并且能够最小化混淆或错误。

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

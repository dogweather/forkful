---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:47.690329-07:00
description: "YAML\uFF0C\u662F\u201CYAML Ain't Markup Language\u201D\u7684\u7F29\u5199\
  \uFF0C\u5B83\u662F\u4E00\u79CD\u6613\u4E8E\u9605\u8BFB\u7684\u6570\u636E\u5E8F\u5217\
  \u5316\u683C\u5F0F\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\u5B83\u8FDB\u884C\u914D\u7F6E\
  \u6587\u4EF6\u7F16\u5199\u3001\u6570\u636E\u4EA4\u6362\u4EE5\u53CA\u5728\u9700\u8981\
  \u4EE5\u6613\u4E8E\u7406\u89E3\u7684\u683C\u5F0F\u5B58\u50A8\u6216\u4F20\u8F93\u6570\
  \u636E\u7684\u5E94\u7528\u7A0B\u5E8F\u4E2D\u3002"
lastmod: '2024-03-11T00:14:21.206015-06:00'
model: gpt-4-0125-preview
summary: "YAML\uFF0C\u662F\u201CYAML Ain't Markup Language\u201D\u7684\u7F29\u5199\
  \uFF0C\u5B83\u662F\u4E00\u79CD\u6613\u4E8E\u9605\u8BFB\u7684\u6570\u636E\u5E8F\u5217\
  \u5316\u683C\u5F0F\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\u5B83\u8FDB\u884C\u914D\u7F6E\
  \u6587\u4EF6\u7F16\u5199\u3001\u6570\u636E\u4EA4\u6362\u4EE5\u53CA\u5728\u9700\u8981\
  \u4EE5\u6613\u4E8E\u7406\u89E3\u7684\u683C\u5F0F\u5B58\u50A8\u6216\u4F20\u8F93\u6570\
  \u636E\u7684\u5E94\u7528\u7A0B\u5E8F\u4E2D\u3002"
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
---

{{< edit_this_page >}}

## 什么 & 为什么？

YAML，是“YAML Ain't Markup Language”的缩写，它是一种易于阅读的数据序列化格式。程序员使用它进行配置文件编写、数据交换以及在需要以易于理解的格式存储或传输数据的应用程序中。

## 如何操作：

在 Dart 中，处理 YAML 通常涉及使用第三方库，因为该语言不包括内置的 YAML 解析功能。一个受欢迎的选择是 `yaml` 包。首先，您需要将此包添加到您的 `pubspec.yaml` 中：

```yaml
dependencies:
  yaml: ^3.1.0
```

记得运行 `pub get` 来获取包。

### 读取 YAML

要读取一个 YAML 文件，首先，导入 `yaml` 包，然后使用 `loadYaml` 函数：

```dart
import 'package:yaml/yaml.dart';
import 'dart:io';

void main() {
  final file = File('config.yaml').readAsStringSync();
  final yamlMap = loadYaml(file);

  print(yamlMap['name']); // 输出：John Doe
}

```

假设您的 `config.yaml` 文件如下所示：

```yaml
name: John Doe
age: 30
```

### 编写 YAML

尽管 `yaml` 包非常适合解析，但它不支持编写 YAML。为此，您可能需要手动将数据转换为 YAML，或使用另一个可用的包。或者，更直接的方式是，管理您的数据转换并将它们以符合 YAML 语法的字符串输出：

```dart
Map<String, dynamic> data = {
  'name': 'Jane Doe',
  'age': 29,
};

String toYamlString(Map<String, dynamic> map) {
  String yaml = '';
  map.forEach((key, value) {
    yaml += '$key: $value\n';
  });
  return yaml;
}

void main() {
  print(toYamlString(data)); // 输出：name: Jane Doe
                             //         age: 29
}
```

这是一种基础方法，可能不适合复杂的数据结构或特殊的 YAML 功能。对于复杂需求，您可能需要寻找或贡献一个更全面的 Dart 包。

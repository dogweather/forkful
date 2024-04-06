---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:47.690329-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 Dart \u4E2D\uFF0C\u5904\u7406\
  \ YAML \u901A\u5E38\u6D89\u53CA\u4F7F\u7528\u7B2C\u4E09\u65B9\u5E93\uFF0C\u56E0\u4E3A\
  \u8BE5\u8BED\u8A00\u4E0D\u5305\u62EC\u5185\u7F6E\u7684 YAML \u89E3\u6790\u529F\u80FD\
  \u3002\u4E00\u4E2A\u53D7\u6B22\u8FCE\u7684\u9009\u62E9\u662F `yaml` \u5305\u3002\
  \u9996\u5148\uFF0C\u60A8\u9700\u8981\u5C06\u6B64\u5305\u6DFB\u52A0\u5230\u60A8\u7684\
  \ `pubspec.yaml` \u4E2D\uFF1A."
lastmod: '2024-04-05T22:38:46.605656-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 Dart \u4E2D\uFF0C\u5904\u7406 YAML\
  \ \u901A\u5E38\u6D89\u53CA\u4F7F\u7528\u7B2C\u4E09\u65B9\u5E93\uFF0C\u56E0\u4E3A\
  \u8BE5\u8BED\u8A00\u4E0D\u5305\u62EC\u5185\u7F6E\u7684 YAML \u89E3\u6790\u529F\u80FD\
  \u3002\u4E00\u4E2A\u53D7\u6B22\u8FCE\u7684\u9009\u62E9\u662F `yaml` \u5305\u3002\
  \u9996\u5148\uFF0C\u60A8\u9700\u8981\u5C06\u6B64\u5305\u6DFB\u52A0\u5230\u60A8\u7684\
  \ `pubspec.yaml` \u4E2D\uFF1A."
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
weight: 41
---

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

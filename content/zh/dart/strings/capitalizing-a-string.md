---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:30.804580-07:00
description: "\u5C06\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\u5927\u5199\u6D89\u53CA\u5C06\
  \u5355\u8BCD\u6216\u6574\u4E2A\u53E5\u5B50\u7684\u7B2C\u4E00\u4E2A\u5B57\u6BCD\u4FEE\
  \u6539\u4E3A\u5927\u5199\uFF0C\u540C\u65F6\u4FDD\u6301\u5176\u4F59\u5B57\u7B26\u4E0D\
  \u53D8\u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\u4F7F\u7528\u8FD9\u79CD\u6280\u672F\u6765\
  \u683C\u5F0F\u5316\u7528\u6237\u8F93\u5165\u6216\u663E\u793A\u6587\u672C\uFF0C\u4EE5\
  \u786E\u4FDD\u4E00\u81F4\u6027\u6216\u9075\u5B88\u7528\u6237\u754C\u9762\u4E2D\u7684\
  \u8BED\u6CD5\u89C4\u5219\u3002"
lastmod: '2024-03-11T00:14:21.153268-06:00'
model: gpt-4-0125-preview
summary: "\u5C06\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\u5927\u5199\u6D89\u53CA\u5C06\
  \u5355\u8BCD\u6216\u6574\u4E2A\u53E5\u5B50\u7684\u7B2C\u4E00\u4E2A\u5B57\u6BCD\u4FEE\
  \u6539\u4E3A\u5927\u5199\uFF0C\u540C\u65F6\u4FDD\u6301\u5176\u4F59\u5B57\u7B26\u4E0D\
  \u53D8\u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\u4F7F\u7528\u8FD9\u79CD\u6280\u672F\u6765\
  \u683C\u5F0F\u5316\u7528\u6237\u8F93\u5165\u6216\u663E\u793A\u6587\u672C\uFF0C\u4EE5\
  \u786E\u4FDD\u4E00\u81F4\u6027\u6216\u9075\u5B88\u7528\u6237\u754C\u9762\u4E2D\u7684\
  \u8BED\u6CD5\u89C4\u5219\u3002"
title: "\u5B57\u7B26\u4E32\u5927\u5199\u8F6C\u6362"
---

{{< edit_this_page >}}

## 什么 & 为什么？

将字符串首字母大写涉及将单词或整个句子的第一个字母修改为大写，同时保持其余字符不变。程序员经常使用这种技术来格式化用户输入或显示文本，以确保一致性或遵守用户界面中的语法规则。

## 如何操作：

### 使用 Dart 的内置方法

Dart 为字符串操作提供了简单直接的方法。要大写单词或句子，你通常会取第一个字符，将其转换为大写，然后与字符串的其余部分连接起来。这是你可以实现它的方式：

```dart
String capitalize(String text) {
  if (text.isEmpty) return text;
  return text[0].toUpperCase() + text.substring(1).toLowerCase();
}

void main() {
  var example = "hello world";
  print(capitalize(example)); // 输出: Hello world
}
```

### 每个单词首字母大写

要将字符串中每个单词的第一个字母大写，你可以将字符串分割成单词，将每个单词大写，然后再将它们连接起来：

```dart
String capitalizeWords(String text) {
  return text.split(' ').map(capitalize).join(' ');
}

void main() {
  var example = "hello dart enthusiasts";
  print(capitalizeWords(example)); // 输出: Hello Dart Enthusiasts
}
```

### 使用第三方库

虽然 Dart 的标准库满足了基本需求，但某些任务使用第三方包可能更加便利。一个受欢迎的选择，用于扩展字符串操作功能，包括大写化，是 [`recase`](https://pub.dev/packages/recase) 包。将其添加到项目的 `pubspec.yaml` 后，你可以轻松地大写字符串以及执行其他功能：

```dart
import 'package:recase/recase.dart';

void main() {
  var example = "hello world";
  var rc = ReCase(example);

  print(rc.titleCase); // 输出: Hello World
}
```

使用 `recase`，你可以大写单个单词、整个句子，甚至遵循其他大小写约定，而无需手动处理字符串转换。

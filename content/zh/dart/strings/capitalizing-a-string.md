---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:30.804580-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Dart \u4E3A\u5B57\u7B26\u4E32\u64CD\u4F5C\
  \u63D0\u4F9B\u4E86\u7B80\u5355\u76F4\u63A5\u7684\u65B9\u6CD5\u3002\u8981\u5927\u5199\
  \u5355\u8BCD\u6216\u53E5\u5B50\uFF0C\u4F60\u901A\u5E38\u4F1A\u53D6\u7B2C\u4E00\u4E2A\
  \u5B57\u7B26\uFF0C\u5C06\u5176\u8F6C\u6362\u4E3A\u5927\u5199\uFF0C\u7136\u540E\u4E0E\
  \u5B57\u7B26\u4E32\u7684\u5176\u4F59\u90E8\u5206\u8FDE\u63A5\u8D77\u6765\u3002\u8FD9\
  \u662F\u4F60\u53EF\u4EE5\u5B9E\u73B0\u5B83\u7684\u65B9\u5F0F\uFF1A."
lastmod: '2024-03-13T22:44:47.395849-06:00'
model: gpt-4-0125-preview
summary: "Dart \u4E3A\u5B57\u7B26\u4E32\u64CD\u4F5C\u63D0\u4F9B\u4E86\u7B80\u5355\u76F4\
  \u63A5\u7684\u65B9\u6CD5\u3002\u8981\u5927\u5199\u5355\u8BCD\u6216\u53E5\u5B50\uFF0C\
  \u4F60\u901A\u5E38\u4F1A\u53D6\u7B2C\u4E00\u4E2A\u5B57\u7B26\uFF0C\u5C06\u5176\u8F6C\
  \u6362\u4E3A\u5927\u5199\uFF0C\u7136\u540E\u4E0E\u5B57\u7B26\u4E32\u7684\u5176\u4F59\
  \u90E8\u5206\u8FDE\u63A5\u8D77\u6765\u3002\u8FD9\u662F\u4F60\u53EF\u4EE5\u5B9E\u73B0\
  \u5B83\u7684\u65B9\u5F0F\uFF1A."
title: "\u5B57\u7B26\u4E32\u5927\u5199\u8F6C\u6362"
weight: 2
---

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

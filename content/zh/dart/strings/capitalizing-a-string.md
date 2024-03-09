---
title:                "字符串大写转换"
date:                  2024-03-08T21:53:30.804580-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
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

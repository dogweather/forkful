---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:32.031276-07:00
description: "\u5982\u4F55\u64CD\u4F5C: Dart \u901A\u8FC7\u4F7F\u7528 `length` \u5C5E\
  \u6027\u4F7F\u5F97\u83B7\u53D6\u5B57\u7B26\u4E32\u957F\u5EA6\u53D8\u5F97\u7B80\u5355\
  \u76F4\u63A5\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\u57FA\u672C\u793A\u4F8B\uFF1A."
lastmod: '2024-03-13T22:44:47.406703-06:00'
model: gpt-4-0125-preview
summary: "Dart \u901A\u8FC7\u4F7F\u7528 `length` \u5C5E\u6027\u4F7F\u5F97\u83B7\u53D6\
  \u5B57\u7B26\u4E32\u957F\u5EA6\u53D8\u5F97\u7B80\u5355\u76F4\u63A5\u3002\u8FD9\u91CC\
  \u6709\u4E00\u4E2A\u57FA\u672C\u793A\u4F8B\uFF1A."
title: "\u67E5\u627E\u5B57\u7B26\u4E32\u7684\u957F\u5EA6"
weight: 7
---

## 如何操作:
Dart 通过使用 `length` 属性使得获取字符串长度变得简单直接。这里有一个基本示例：

```dart
void main() {
  String myString = "Hello, Dart!";
  print("The length of '\(myString)' is: \(myString.length)");
  // 输出: The length of 'Hello, Dart!' is: 12
}
```
此属性计算字符串中 UTF-16 代码单元的数量，这对于大多数常见用例而言，对应于字符串的长度。

对于更细微的文本处理，尤其是涉及超出基础多语言平面（BMP）的 Unicode 字符，请考虑使用 `characters` 包来计算字形簇，这可以更准确地代表用户感知到的字符。

首先，在你的 `pubspec.yaml` 中添加 `characters`：

```yaml
dependencies:
  characters: ^1.2.0
```

然后，像这样使用它：

```dart
import 'package:characters/characters.dart';

void main() {
  String myEmojiString = "👨‍👩‍👧‍👦 family";
  print("The length of '\(myEmojiString)' is: \(myEmojiString.characters.length)");
  // 输出: The length of '👨‍👩‍👧‍👦 family' is: 8
}
```

在此示例中，`myEmojiString.characters.length` 给出的长度是根据 Unicode 字形簇来计算的，这对于包含复杂字符的字符串（如表情符号或组合字符标记）来说，是更准确的表示方式。

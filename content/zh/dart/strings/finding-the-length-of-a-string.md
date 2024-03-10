---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:32.031276-07:00
description: "\u5728 Dart \u4E2D\u67E5\u627E\u5B57\u7B26\u4E32\u7684\u957F\u5EA6\u6D89\
  \u53CA\u786E\u5B9A\u7ED9\u5B9A\u5B57\u7B26\u4E32\u4E2D\u4EE3\u7801\u5355\u5143\uFF08\
  \u672C\u8D28\u4E0A\uFF0C\u5982\u679C\u7B80\u5355\u5730\u8003\u8651\uFF0C\u5219\u4E3A\
  \u5B57\u7B26\u6570\uFF09\u7684\u6570\u91CF\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\
  \u662F\u4E3A\u4E86\u66F4\u7CBE\u786E\u5730\u64CD\u4F5C\u5B57\u7B26\u4E32\uFF0C\u4F8B\
  \u5982\u9A8C\u8BC1\u8F93\u5165\u3001\u622A\u65AD\u663E\u793A\u6587\u672C\u6216\u5904\
  \u7406\u957F\u5EA6\u5F88\u91CD\u8981\u7684\u6570\u636E\u683C\u5F0F\uFF08\u4F8B\u5982\
  \uFF0C\u5E26\u957F\u5EA6\u524D\u7F00\u7684\u6D88\u606F\u7684\u534F\u8BAE\uFF09\u3002"
lastmod: '2024-03-09T21:06:11.734708-07:00'
model: gpt-4-0125-preview
summary: "\u5728 Dart \u4E2D\u67E5\u627E\u5B57\u7B26\u4E32\u7684\u957F\u5EA6\u6D89\
  \u53CA\u786E\u5B9A\u7ED9\u5B9A\u5B57\u7B26\u4E32\u4E2D\u4EE3\u7801\u5355\u5143\uFF08\
  \u672C\u8D28\u4E0A\uFF0C\u5982\u679C\u7B80\u5355\u5730\u8003\u8651\uFF0C\u5219\u4E3A\
  \u5B57\u7B26\u6570\uFF09\u7684\u6570\u91CF\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\
  \u662F\u4E3A\u4E86\u66F4\u7CBE\u786E\u5730\u64CD\u4F5C\u5B57\u7B26\u4E32\uFF0C\u4F8B\
  \u5982\u9A8C\u8BC1\u8F93\u5165\u3001\u622A\u65AD\u663E\u793A\u6587\u672C\u6216\u5904\
  \u7406\u957F\u5EA6\u5F88\u91CD\u8981\u7684\u6570\u636E\u683C\u5F0F\uFF08\u4F8B\u5982\
  \uFF0C\u5E26\u957F\u5EA6\u524D\u7F00\u7684\u6D88\u606F\u7684\u534F\u8BAE\uFF09\u3002"
title: "\u67E5\u627E\u5B57\u7B26\u4E32\u7684\u957F\u5EA6"
---

{{< edit_this_page >}}

## 什么 & 为什么?
在 Dart 中查找字符串的长度涉及确定给定字符串中代码单元（本质上，如果简单地考虑，则为字符数）的数量。程序员这样做是为了更精确地操作字符串，例如验证输入、截断显示文本或处理长度很重要的数据格式（例如，带长度前缀的消息的协议）。

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

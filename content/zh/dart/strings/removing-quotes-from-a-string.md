---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:03.098442-07:00
description: "\u5728 Dart \u4E2D\u79FB\u9664\u5B57\u7B26\u4E32\u4E2D\u7684\u5F15\u53F7\
  \u6D89\u53CA\u53BB\u6389\u5B57\u7B26\u4E32\u5F00\u5934\u548C\u7ED3\u5C3E\u7684\u53CC\
  \u5F15\u53F7 (\") \u6216\u5355\u5F15\u53F7 (')\uFF0C\u8FD9\u5BF9\u4E8E\u6570\u636E\
  \u6E05\u6D17\u6216\u4E3A\u8FDB\u4E00\u6B65\u5904\u7406\u51C6\u5907\u5B57\u7B26\u4E32\
  \u975E\u5E38\u6709\u7528\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\
  \u89C4\u8303\u6570\u636E\u8F93\u5165\uFF0C\u786E\u4FDD\u6570\u636E\u5B58\u50A8\u7684\
  \u4E00\u81F4\u6027\uFF0C\u6216\u5F53\u4E0E\u53EF\u80FD\u8FD4\u56DE\u5E26\u5F15\u53F7\
  \u683C\u5F0F\u6570\u636E\u7684 API \u4EA4\u4E92\u65F6\u3002"
lastmod: '2024-03-13T22:44:47.402654-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Dart \u4E2D\u79FB\u9664\u5B57\u7B26\u4E32\u4E2D\u7684\u5F15\u53F7\
  \u6D89\u53CA\u53BB\u6389\u5B57\u7B26\u4E32\u5F00\u5934\u548C\u7ED3\u5C3E\u7684\u53CC\
  \u5F15\u53F7 (\") \u6216\u5355\u5F15\u53F7 (')\uFF0C\u8FD9\u5BF9\u4E8E\u6570\u636E\
  \u6E05\u6D17\u6216\u4E3A\u8FDB\u4E00\u6B65\u5904\u7406\u51C6\u5907\u5B57\u7B26\u4E32\
  \u975E\u5E38\u6709\u7528\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\
  \u89C4\u8303\u6570\u636E\u8F93\u5165\uFF0C\u786E\u4FDD\u6570\u636E\u5B58\u50A8\u7684\
  \u4E00\u81F4\u6027\uFF0C\u6216\u5F53\u4E0E\u53EF\u80FD\u8FD4\u56DE\u5E26\u5F15\u53F7\
  \u683C\u5F0F\u6570\u636E\u7684 API \u4EA4\u4E92\u65F6\u3002"
title: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u79FB\u9664\u5F15\u53F7"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在 Dart 中移除字符串中的引号涉及去掉字符串开头和结尾的双引号 (") 或单引号 (')，这对于数据清洗或为进一步处理准备字符串非常有用。程序员这样做是为了规范数据输入，确保数据存储的一致性，或当与可能返回带引号格式数据的 API 交互时。

## 如何操作：
Dart 提供了使用内置字符串方法来移除字符串中的引号的直接方法，无需第三方库。

### 示例 1：使用 `replaceFirst` 和 `replaceAll`
如果你处理的字符串是以引号开始和结束的，你可以使用 `replaceFirst` 和 `replaceAll` 方法来移除它们。

```dart
String quotedString = '"Hello, World!"';
String singleQuotedString = '\'Dart 编程\'';

// 移除双引号
String noDoubleQuotes = quotedString.replaceFirst('"', '').replaceAll('"', '');
print(noDoubleQuotes); // 输出：Hello, World!

// 移除单引号
String noSingleQuotes = singleQuotedString.replaceFirst('\'', '').replaceAll('\'', '');
print(noSingleQuotes); // 输出：Dart 编程
```

### 示例 2：使用 `substring`
当你确定引号确实在字符串的最开始和最末尾时，这种方法很有用。

```dart
String quotedString = '"Flutter 开发"';
// 在移除前检查是否以引号开始和结束，以避免错误
if (quotedString.startsWith('"') && quotedString.endsWith('"')) {
  quotedString = quotedString.substring(1, quotedString.length - 1);
}
print(quotedString); // 输出：Flutter 开发
```

### 示例 3：自定义扩展方法
为了更多的可重用性，特别是如果你的项目涉及频繁的引号移除，请考虑在 `String` 上创建一个自定义扩展。

```dart
extension UnquoteString on String {
  String unquote() {
    var str = this;
    if (str.startsWith('"') && str.endsWith('"') || str.startsWith('\'') && str.endsWith('\'')) {
      str = str.substring(1, str.length - 1);
    }
    return str;
  }
}

void main() {
  String doubleQuoted = '"This is Dart"';
  String singleQuoted = '\'This is awesome\'';
  print(doubleQuoted.unquote()); // 输出：This is Dart
  print(singleQuoted.unquote()); // 输出：This is awesome
}
```

这些方法应该帮助你在 Dart 中有效地移除字符串中的引号，增强你的数据处理和准备工作流程。

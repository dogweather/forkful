---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:03.098442-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Dart \u63D0\u4F9B\u4E86\u4F7F\u7528\u5185\
  \u7F6E\u5B57\u7B26\u4E32\u65B9\u6CD5\u6765\u79FB\u9664\u5B57\u7B26\u4E32\u4E2D\u7684\
  \u5F15\u53F7\u7684\u76F4\u63A5\u65B9\u6CD5\uFF0C\u65E0\u9700\u7B2C\u4E09\u65B9\u5E93\
  \u3002"
lastmod: '2024-04-05T22:38:46.566220-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A Dart \u63D0\u4F9B\u4E86\u4F7F\u7528\u5185\
  \u7F6E\u5B57\u7B26\u4E32\u65B9\u6CD5\u6765\u79FB\u9664\u5B57\u7B26\u4E32\u4E2D\u7684\
  \u5F15\u53F7\u7684\u76F4\u63A5\u65B9\u6CD5\uFF0C\u65E0\u9700\u7B2C\u4E09\u65B9\u5E93\
  \u3002"
title: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u79FB\u9664\u5F15\u53F7"
weight: 9
---

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

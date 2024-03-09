---
title:                "从字符串中移除引号"
date:                  2024-03-08T21:56:03.098442-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
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

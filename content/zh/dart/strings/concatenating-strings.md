---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:47.597222-07:00
description: "\u5982\u4F55\u505A\uFF1A Dart \u63D0\u4F9B\u4E86\u51E0\u79CD\u76F4\u63A5\
  \u7684\u65B9\u5F0F\u6765\u8FDE\u63A5\u5B57\u7B26\u4E32\u3002\u4E0B\u9762\u662F\u6700\
  \u5E38\u89C1\u7684\u65B9\u6CD5\uFF1A `+` \u8FD0\u7B97\u7B26\u662F\u8FDE\u63A5\u5B57\
  \u7B26\u4E32\u6700\u76F4\u89C2\u7684\u65B9\u5F0F\u3002"
lastmod: '2024-03-13T22:44:47.407872-06:00'
model: gpt-4-0125-preview
summary: "Dart \u63D0\u4F9B\u4E86\u51E0\u79CD\u76F4\u63A5\u7684\u65B9\u5F0F\u6765\u8FDE\
  \u63A5\u5B57\u7B26\u4E32\u3002\u4E0B\u9762\u662F\u6700\u5E38\u89C1\u7684\u65B9\u6CD5\
  \uFF1A\n\n`+` \u8FD0\u7B97\u7B26\u662F\u8FDE\u63A5\u5B57\u7B26\u4E32\u6700\u76F4\
  \u89C2\u7684\u65B9\u5F0F."
title: "\u5B57\u7B26\u4E32\u62FC\u63A5"
weight: 3
---

## 如何做：
Dart 提供了几种直接的方式来连接字符串。下面是最常见的方法：

### 使用 `+` 运算符
`+` 运算符是连接字符串最直观的方式。
```dart
String greeting = 'Hello, ' + 'World!';
print(greeting); // 输出：Hello, World!
```

### 使用 `concat()` 方法
尽管 Dart 没有类似其他语言的 `concat()` 方法，但可以通过使用 `+` 或以下方法来实现相同的目的。

### 使用字符串插值
字符串插值允许在字符串内直接嵌入变量。它对于组合字符串和表达式非常有效。
```dart
String user = 'Jane';
String message = 'Welcome, $user!';
print(message); // 输出：Welcome, Jane!
```

### 使用 `join()` 方法
当你有一系列字符串想要连接时，`join()` 方法非常有用。
```dart
var words = ['Hello', 'from', 'Dart'];
String sentence = words.join(' '); // 使用空格分隔符连接。
print(sentence); // 输出：Hello from Dart
```

### 使用 StringBuffer
`StringBuffer` 对于多次连接，特别是在循环中非常高效。
```dart
var words = ['Dart', 'is', 'fun'];
StringBuffer buffer = StringBuffer();
for (String word in words) {
  buffer.write(word); // 将每个单词附加到缓冲区。
  buffer.write(' '); // 可选地添加一个空格。
}
String sentence = buffer.toString().trim(); // 转换为字符串并删除尾随空格。
print(sentence); // 输出：Dart is fun
```

### 第三方库
虽然 Dart 的标准库通常足以处理字符串连接任务，但像 `quiver` 这样的第三方库提供的实用工具可以补充 Dart 内置的功能。例如，对于高级场景，可以探索 `quiver` 的 `concat()` 或 `merge()` 函数。然而，除非你有它们无法覆盖的特定需求，否则应坚持使用 Dart 强大的内置选项。

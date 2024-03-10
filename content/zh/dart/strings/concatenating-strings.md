---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:47.597222-07:00
description: "\u5728\u7F16\u7A0B\u4E2D\u8FDE\u63A5\u5B57\u7B26\u4E32\u6D89\u53CA\u5C06\
  \u4E24\u4E2A\u6216\u591A\u4E2A\u5B57\u7B26\u4E32\u7EC4\u5408\u6210\u4E00\u4E2A\u3002\
  \u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u8F7B\u677E\u5730\u64CD\u4F5C\
  \u6587\u672C\u6570\u636E\uFF0C\u6784\u9020\u6D88\u606F\uFF0C\u6216\u52A8\u6001\u7EC4\
  \u88C5\u7528\u6237\u754C\u9762\u7684\u5404\u4E2A\u90E8\u5206\u3002"
lastmod: '2024-03-09T21:06:11.735555-07:00'
model: gpt-4-0125-preview
summary: "\u5728\u7F16\u7A0B\u4E2D\u8FDE\u63A5\u5B57\u7B26\u4E32\u6D89\u53CA\u5C06\
  \u4E24\u4E2A\u6216\u591A\u4E2A\u5B57\u7B26\u4E32\u7EC4\u5408\u6210\u4E00\u4E2A\u3002\
  \u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u8F7B\u677E\u5730\u64CD\u4F5C\
  \u6587\u672C\u6570\u636E\uFF0C\u6784\u9020\u6D88\u606F\uFF0C\u6216\u52A8\u6001\u7EC4\
  \u88C5\u7528\u6237\u754C\u9762\u7684\u5404\u4E2A\u90E8\u5206\u3002"
title: "\u5B57\u7B26\u4E32\u62FC\u63A5"
---

{{< edit_this_page >}}

## 什么和为什么？
在编程中连接字符串涉及将两个或多个字符串组合成一个。程序员这样做是为了轻松地操作文本数据，构造消息，或动态组装用户界面的各个部分。

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

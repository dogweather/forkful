---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:33.306743-07:00
description: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u662F\u6307\u57FA\u4E8E\u4F4D\u7F6E\
  \u6216\u6A21\u5F0F\uFF0C\u4ECE\u5B57\u7B26\u4E32\u4E2D\u68C0\u7D22\u7279\u5B9A\u90E8\
  \u5206\u7684\u64CD\u4F5C\u3002\u7A0B\u5E8F\u5458\u8FDB\u884C\u8FD9\u9879\u64CD\u4F5C\
  \u662F\u4E3A\u4E86\u5B8C\u6210\u4EFB\u52A1\uFF0C\u5982\u89E3\u6790\u7528\u6237\u8F93\
  \u5165\u3001\u6570\u636E\u64CD\u4F5C\u6216\u4ECE\u8F83\u5927\u7684\u6587\u672C\u6E90\
  \u4E2D\u63D0\u53D6\u76F8\u5173\u4FE1\u606F\u3002"
lastmod: '2024-03-13T22:44:47.403901-06:00'
model: gpt-4-0125-preview
summary: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u662F\u6307\u57FA\u4E8E\u4F4D\u7F6E\
  \u6216\u6A21\u5F0F\uFF0C\u4ECE\u5B57\u7B26\u4E32\u4E2D\u68C0\u7D22\u7279\u5B9A\u90E8\
  \u5206\u7684\u64CD\u4F5C\u3002\u7A0B\u5E8F\u5458\u8FDB\u884C\u8FD9\u9879\u64CD\u4F5C\
  \u662F\u4E3A\u4E86\u5B8C\u6210\u4EFB\u52A1\uFF0C\u5982\u89E3\u6790\u7528\u6237\u8F93\
  \u5165\u3001\u6570\u636E\u64CD\u4F5C\u6216\u4ECE\u8F83\u5927\u7684\u6587\u672C\u6E90\
  \u4E2D\u63D0\u53D6\u76F8\u5173\u4FE1\u606F\u3002."
title: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32"
weight: 6
---

## 如何操作：
在 Dart 中，您可以使用各种方法来提取子字符串，例如 `substring()`、`split()` 和正则表达式。每种方法都服务于不同的目的，并在处理字符串时提供灵活性。

### 使用 `substring()`：
`substring()` 方法很直接。您指定开始（并可选地指定结束）索引来切割字符串。

```dart
void main() {
  String example = "Hello, World!";
  String result = example.substring(7, 12);
  print(result); // 输出：World
}
```

### 使用 `split()`：
基于模式（如空格或逗号）将字符串分割为子字符串列表，然后通过索引访问子字符串。

```dart
void main() {
  String example = "Dart is fun";
  List<String> parts = example.split(' ');
  String result = parts[1]; // 通过索引访问
  print(result); // 输出：is
}
```

### 使用正则表达式：
对于复杂模式，Dart 的 `RegExp` 类非常强大。使用它来匹配模式并提取子字符串。

```dart
void main() {
  String example = "Email: example@mail.com";
  RegExp regExp = RegExp(r"\b\w+@\w+\.\w+\b");
  String email = regExp.stringMatch(example)!;
  print(email); // 输出：example@mail.com
}
```

### 第三方库：
尽管 Dart 的标准库相当有能力，但您可能会遇到第三方库可以简化任务的情况。这里没有特别推荐用于字符串操作和模式匹配的流行选择，因为 Dart 的内置功能往往就足够了。然而，总是检查 [pub.dev](https://pub.dev) 以查找可能更适合您具体需求的任何库。

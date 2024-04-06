---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:33.306743-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 Dart \u4E2D\uFF0C\u60A8\u53EF\u4EE5\
  \u4F7F\u7528\u5404\u79CD\u65B9\u6CD5\u6765\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\uFF0C\
  \u4F8B\u5982 `substring()`\u3001`split()` \u548C\u6B63\u5219\u8868\u8FBE\u5F0F\u3002\
  \u6BCF\u79CD\u65B9\u6CD5\u90FD\u670D\u52A1\u4E8E\u4E0D\u540C\u7684\u76EE\u7684\uFF0C\
  \u5E76\u5728\u5904\u7406\u5B57\u7B26\u4E32\u65F6\u63D0\u4F9B\u7075\u6D3B\u6027\u3002"
lastmod: '2024-04-05T21:53:47.734680-06:00'
model: gpt-4-0125-preview
summary: ''
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

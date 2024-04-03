---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:00.261239-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728Dart\u4E2D\uFF0C\u5B57\u7B26\u4E32\
  \u63D2\u503C\u975E\u5E38\u76F4\u63A5\uFF0C\u5229\u7528`$`\u7B26\u53F7\u76F4\u63A5\
  \u5728\u5B57\u7B26\u4E32\u5B57\u9762\u91CF\u4E2D\u63D2\u503C\u8868\u8FBE\u5F0F\uFF1A\
  ."
lastmod: '2024-03-13T22:44:47.399746-06:00'
model: gpt-4-0125-preview
summary: "\u5728Dart\u4E2D\uFF0C\u5B57\u7B26\u4E32\u63D2\u503C\u975E\u5E38\u76F4\u63A5\
  \uFF0C\u5229\u7528`$`\u7B26\u53F7\u76F4\u63A5\u5728\u5B57\u7B26\u4E32\u5B57\u9762\
  \u91CF\u4E2D\u63D2\u503C\u8868\u8FBE\u5F0F\uFF1A."
title: "\u5B57\u7B26\u4E32\u63D2\u503C"
weight: 8
---

## 如何操作：
在Dart中，字符串插值非常直接，利用`$`符号直接在字符串字面量中插值表达式：

```dart
void main() {
  String name = 'Dart';
  int year = 2023;
  // 简单变量插值
  print('Learning $name in $year!');
  // 输出：Learning Dart in 2023!
  
  // 插值表达式
  print('In two years, it will be ${year + 2}.');
  // 输出：In two years, it will be 2025.
}
```

在你有更复杂的表达式或想在字符串本身执行操作的情况下，将表达式封闭在`${}`中。Dart没有任何特别流行的第三方库专门用于字符串插值，因为它在本地就能很好地处理各种复杂的情况。

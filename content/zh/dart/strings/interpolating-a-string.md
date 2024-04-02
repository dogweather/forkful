---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:00.261239-07:00
description: "\u5B57\u7B26\u4E32\u63D2\u503C\u662F\u5C06\u53D8\u91CF\u503C\u76F4\u63A5\
  \u6CE8\u5165\u5230\u5B57\u7B26\u4E32\u4E2D\u7684\u8FC7\u7A0B\uFF0C\u5F80\u5F80\u7528\
  \u4E8E\u521B\u5EFA\u6709\u610F\u4E49\u7684\u6D88\u606F\u800C\u4E0D\u9700\u7E41\u7410\
  \u7684\u4E32\u8054\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u4EE3\
  \u7801\u66F4\u6E05\u6670\u3001\u53EF\u8BFB\u6027\u66F4\u9AD8\uFF0C\u540C\u65F6\u4E5F\
  \u662F\u4E3A\u4E86\u9632\u6B62\u5728\u590D\u6742\u7684\u5B57\u7B26\u4E32\u4E32\u8054\
  \u4E2D\u5BB9\u6613\u51FA\u73B0\u7684\u9519\u8BEF\u3002"
lastmod: '2024-03-13T22:44:47.399746-06:00'
model: gpt-4-0125-preview
summary: "\u5B57\u7B26\u4E32\u63D2\u503C\u662F\u5C06\u53D8\u91CF\u503C\u76F4\u63A5\
  \u6CE8\u5165\u5230\u5B57\u7B26\u4E32\u4E2D\u7684\u8FC7\u7A0B\uFF0C\u5F80\u5F80\u7528\
  \u4E8E\u521B\u5EFA\u6709\u610F\u4E49\u7684\u6D88\u606F\u800C\u4E0D\u9700\u7E41\u7410\
  \u7684\u4E32\u8054\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u4EE3\
  \u7801\u66F4\u6E05\u6670\u3001\u53EF\u8BFB\u6027\u66F4\u9AD8\uFF0C\u540C\u65F6\u4E5F\
  \u662F\u4E3A\u4E86\u9632\u6B62\u5728\u590D\u6742\u7684\u5B57\u7B26\u4E32\u4E32\u8054\
  \u4E2D\u5BB9\u6613\u51FA\u73B0\u7684\u9519\u8BEF\u3002"
title: "\u5B57\u7B26\u4E32\u63D2\u503C"
weight: 8
---

## 什么及为何？

字符串插值是将变量值直接注入到字符串中的过程，往往用于创建有意义的消息而不需繁琐的串联。程序员这么做是为了代码更清晰、可读性更高，同时也是为了防止在复杂的字符串串联中容易出现的错误。

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

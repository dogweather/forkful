---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:57.235455-07:00
description: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u6D89\u53CA\u53D6\u51FA\u5B57\u7B26\
  \u4E32\u7684\u4E00\u90E8\u5206\u2014\u2014\u672C\u8D28\u4E0A\u662F\u4ECE\u73B0\u6709\
  \u7684\u5B57\u7B26\u4E32\u4E2D\u521B\u5EFA\u4E00\u4E2A\u65B0\u7684\u5B57\u7B26\u4E32\
  \u3002\u7A0B\u5E8F\u5458\u51FA\u4E8E\u5404\u79CD\u539F\u56E0\u6267\u884C\u6B64\u64CD\
  \u4F5C\uFF0C\u5305\u62EC\u6570\u636E\u89E3\u6790\u3001\u7528\u6237\u754C\u9762\u7684\
  \u6587\u672C\u64CD\u4F5C\uFF0C\u6216\u8005\u4E3A\u5404\u79CD\u5E94\u7528\u7A0B\u5E8F\
  \u5904\u7406\u8F93\u5165\uFF0C\u4F7F\u5F97\u5B50\u5B57\u7B26\u4E32\u63D0\u53D6\u6210\
  \u4E3A\u4EFB\u4F55\u811A\u672C\u5DE5\u5177\u7BB1\u4E2D\u7684\u591A\u529F\u80FD\u5DE5\
  \u5177\u3002"
lastmod: '2024-03-11T00:14:20.948261-06:00'
model: gpt-4-0125-preview
summary: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u6D89\u53CA\u53D6\u51FA\u5B57\u7B26\
  \u4E32\u7684\u4E00\u90E8\u5206\u2014\u2014\u672C\u8D28\u4E0A\u662F\u4ECE\u73B0\u6709\
  \u7684\u5B57\u7B26\u4E32\u4E2D\u521B\u5EFA\u4E00\u4E2A\u65B0\u7684\u5B57\u7B26\u4E32\
  \u3002\u7A0B\u5E8F\u5458\u51FA\u4E8E\u5404\u79CD\u539F\u56E0\u6267\u884C\u6B64\u64CD\
  \u4F5C\uFF0C\u5305\u62EC\u6570\u636E\u89E3\u6790\u3001\u7528\u6237\u754C\u9762\u7684\
  \u6587\u672C\u64CD\u4F5C\uFF0C\u6216\u8005\u4E3A\u5404\u79CD\u5E94\u7528\u7A0B\u5E8F\
  \u5904\u7406\u8F93\u5165\uFF0C\u4F7F\u5F97\u5B50\u5B57\u7B26\u4E32\u63D0\u53D6\u6210\
  \u4E3A\u4EFB\u4F55\u811A\u672C\u5DE5\u5177\u7BB1\u4E2D\u7684\u591A\u529F\u80FD\u5DE5\
  \u5177\u3002"
title: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32"
---

{{< edit_this_page >}}

## 什么 & 为什么？

提取子字符串涉及取出字符串的一部分——本质上是从现有的字符串中创建一个新的字符串。程序员出于各种原因执行此操作，包括数据解析、用户界面的文本操作，或者为各种应用程序处理输入，使得子字符串提取成为任何脚本工具箱中的多功能工具。

## 如何操作：

在基于现代JavaScript的Google Apps Script中，可以通过几种方法实现子字符串提取，包括 `substring()`、`substr()` 和 `slice()`。每种方法都有其细微差别，但它们都旨在从字符串中拉取指定字符。

```javascript
// 使用 substring() 的例子
var str = "Hello, world!";
var result = str.substring(0, 5);
console.log(result); // 输出：Hello

// 使用 substr() 的例子
var resultSubstr = str.substr(7, 5);
console.log(resultSubstr); // 输出：world

// 使用 slice() 的例子
var resultSlice = str.slice(-6);
console.log(resultSlice); // 输出：world!
```

每种方法都需要两个参数：起始位置和终止位置或要提取的字符数，除了 `slice()` 可以接受负索引从末尾开始以外。值得注意的是，这些操作之后原始字符串保持不变，因为它们返回新的字符串值。

## 深入了解

从历史上看，JavaScript中用于提取子字符串的方法由于其名称和功能的相似性而常常令人混淆。然而，在Google Apps Script和现代JavaScript中，通常最常使用 `substring()` 和 `slice()`，而将 `substr()` 视为已弃用。对于编写未来证明代码的人来说，这一点很重要。

`substring()` 和 `slice()` 之间的主要区别在于它们如何处理负索引；`substring()` 视负索引为0，而 `slice()` 可以接受负索引从字符串的末尾开始提取。这使得 `slice()` 特别适用于不确切知道字符串长度或需要从末尾提取时的情况。

决定使用哪种方法进行子字符串提取时，选择往往归结为操作的具体要求（例如，是否需要处理负索引）以及个人或团队的编码标准。虽然没有适用于所有情况的最佳实践，但了解这些细微的差异和性能影响可以帮助做出明智的决策。

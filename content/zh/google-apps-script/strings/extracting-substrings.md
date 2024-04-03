---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:57.235455-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728\u57FA\u4E8E\u73B0\u4EE3JavaScript\u7684\
  Google Apps Script\u4E2D\uFF0C\u53EF\u4EE5\u901A\u8FC7\u51E0\u79CD\u65B9\u6CD5\u5B9E\
  \u73B0\u5B50\u5B57\u7B26\u4E32\u63D0\u53D6\uFF0C\u5305\u62EC `substring()`\u3001\
  `substr()` \u548C `slice()`\u3002\u6BCF\u79CD\u65B9\u6CD5\u90FD\u6709\u5176\u7EC6\
  \u5FAE\u5DEE\u522B\uFF0C\u4F46\u5B83\u4EEC\u90FD\u65E8\u5728\u4ECE\u5B57\u7B26\u4E32\
  \u4E2D\u62C9\u53D6\u6307\u5B9A\u5B57\u7B26\u3002"
lastmod: '2024-03-13T22:44:47.185628-06:00'
model: gpt-4-0125-preview
summary: "\u5728\u57FA\u4E8E\u73B0\u4EE3JavaScript\u7684Google Apps Script\u4E2D\uFF0C\
  \u53EF\u4EE5\u901A\u8FC7\u51E0\u79CD\u65B9\u6CD5\u5B9E\u73B0\u5B50\u5B57\u7B26\u4E32\
  \u63D0\u53D6\uFF0C\u5305\u62EC `substring()`\u3001`substr()` \u548C `slice()`\u3002\
  \u6BCF\u79CD\u65B9\u6CD5\u90FD\u6709\u5176\u7EC6\u5FAE\u5DEE\u522B\uFF0C\u4F46\u5B83\
  \u4EEC\u90FD\u65E8\u5728\u4ECE\u5B57\u7B26\u4E32\u4E2D\u62C9\u53D6\u6307\u5B9A\u5B57\
  \u7B26."
title: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32"
weight: 6
---

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

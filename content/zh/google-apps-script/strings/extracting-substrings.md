---
title:                "提取子字符串"
aliases:
- zh/google-apps-script/extracting-substrings.md
date:                  2024-02-01T21:52:57.235455-07:00
model:                 gpt-4-0125-preview
simple_title:         "提取子字符串"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/google-apps-script/extracting-substrings.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
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

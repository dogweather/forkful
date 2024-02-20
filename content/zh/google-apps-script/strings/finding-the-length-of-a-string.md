---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:53:28.189388-07:00
description: "\u5728 Google Apps \u811A\u672C\u4E2D\u627E\u5230\u4E00\u4E2A\u5B57\u7B26\
  \u4E32\u7684\u957F\u5EA6\uFF0CGoogle Apps \u811A\u672C\u662F\u4E00\u79CD JavaScript\
  \ \u4E91\u811A\u672C\u8BED\u8A00\uFF0C\u5B83\u8BA9\u60A8\u53EF\u4EE5\u81EA\u52A8\
  \u6267\u884C\u8DE8 Google \u4EA7\u54C1\u7684\u4EFB\u52A1\uFF0C\u5176\u76EE\u7684\
  \u662F\u786E\u5B9A\u5B57\u7B26\u4E32\u5305\u542B\u7684\u5B57\u7B26\u6570\u91CF\u3002\
  \u7A0B\u5E8F\u5458\u7ECF\u5E38\u6267\u884C\u6B64\u64CD\u4F5C\u6765\u9A8C\u8BC1\u8F93\
  \u5165\u3001\u904D\u5386\u5B57\u7B26\u6216\u4E3A\u5404\u79CD\u81EA\u52A8\u5316\u4EFB\
  \u52A1\u64CD\u4F5C\u5B57\u7B26\u4E32\uFF0C\u8FD9\u4E9B\u4EFB\u52A1\u5728 Google\u2026"
lastmod: 2024-02-19 22:05:06.265084
model: gpt-4-0125-preview
summary: "\u5728 Google Apps \u811A\u672C\u4E2D\u627E\u5230\u4E00\u4E2A\u5B57\u7B26\
  \u4E32\u7684\u957F\u5EA6\uFF0CGoogle Apps \u811A\u672C\u662F\u4E00\u79CD JavaScript\
  \ \u4E91\u811A\u672C\u8BED\u8A00\uFF0C\u5B83\u8BA9\u60A8\u53EF\u4EE5\u81EA\u52A8\
  \u6267\u884C\u8DE8 Google \u4EA7\u54C1\u7684\u4EFB\u52A1\uFF0C\u5176\u76EE\u7684\
  \u662F\u786E\u5B9A\u5B57\u7B26\u4E32\u5305\u542B\u7684\u5B57\u7B26\u6570\u91CF\u3002\
  \u7A0B\u5E8F\u5458\u7ECF\u5E38\u6267\u884C\u6B64\u64CD\u4F5C\u6765\u9A8C\u8BC1\u8F93\
  \u5165\u3001\u904D\u5386\u5B57\u7B26\u6216\u4E3A\u5404\u79CD\u81EA\u52A8\u5316\u4EFB\
  \u52A1\u64CD\u4F5C\u5B57\u7B26\u4E32\uFF0C\u8FD9\u4E9B\u4EFB\u52A1\u5728 Google\u2026"
title: "\u5BFB\u627E\u5B57\u7B26\u4E32\u7684\u957F\u5EA6"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在 Google Apps 脚本中找到一个字符串的长度，Google Apps 脚本是一种 JavaScript 云脚本语言，它让您可以自动执行跨 Google 产品的任务，其目的是确定字符串包含的字符数量。程序员经常执行此操作来验证输入、遍历字符或为各种自动化任务操作字符串，这些任务在 Google 应用程序中进行。

## 如何操作：
在 Google Apps 脚本中，您可以使用 `.length` 属性找到一个字符串的长度，这与 JavaScript 相似。此属性返回字符串内的字符数，包括空格和特殊字符。这里有一些示例：

```javascript
// 定义一个字符串
var text = "Hello, World!";
// 查找字符串的长度
var length = text.length;
// 记录长度
Logger.log(length); // 输出: 13
```

在处理来自 Google 表单或表格的用户输入的情景中，找出字符串长度有助于数据验证：

```javascript
// 来自 Google 表格中用户的示例字符串输入
var userEntry = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet().getRange("A1").getValue();
// 计算并记录输入的长度
Logger.log(userEntry.length); // 输出取决于单元格 A1 的内容
```

让我们添加一个包含条件的实际示例。如果输入超过某个长度，您可能想要抛出一个错误或警告：

```javascript
var comment = "This is a sample comment that is too long for our database.";
if(comment.length > 50) {
  Logger.log("Error: Your comment should not exceed 50 characters.");
} else {
  Logger.log("Thank you for your submission.");
}
// 输出: Error: Your comment should not exceed 50 characters.
```

## 深入探索
在 Google Apps 脚本的背景下，它是基于 JavaScript 的，`.length` 属性来自 ECMAScript 标准，该标准规定了 JavaScript 的规范。`.length` 属性自 JavaScript 初期以来就是其部分，提供了一种简单的方法来评估字符串的大小。

值得注意的是，Google Apps 脚本是在 Google 的服务器上执行的，而不是在浏览器中。这意味着，当您处理字符串及其长度时，特别是在从 Google 表格或文档检索大型数据集时，由于网络延迟和脚本的运行时限制，执行时间可能会受到影响。

尽管 `.length` 是一种直接且广泛使用的方法来找出字符串的长度，但如果处理多字节字符或需要过滤掉某些类型的字符时，可能需要采用正则表达式或遍历字符串以计数字符的替代策略。然而，对于 Google Apps 脚本中的大多数实际目的，`.length` 提供了一种可靠且高效的方式来确定字符串长度。

始终记住，特别是在 Google Apps 脚本中，要考虑您正在运行代码的上下文。性能和执行限制可能会引导您优化字符串处理程序，包括如何确定它们的长度。

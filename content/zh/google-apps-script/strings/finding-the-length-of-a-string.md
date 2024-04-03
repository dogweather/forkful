---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:53:28.189388-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 Google Apps \u811A\u672C\u4E2D\
  \uFF0C\u60A8\u53EF\u4EE5\u4F7F\u7528 `.length` \u5C5E\u6027\u627E\u5230\u4E00\u4E2A\
  \u5B57\u7B26\u4E32\u7684\u957F\u5EA6\uFF0C\u8FD9\u4E0E JavaScript \u76F8\u4F3C\u3002\
  \u6B64\u5C5E\u6027\u8FD4\u56DE\u5B57\u7B26\u4E32\u5185\u7684\u5B57\u7B26\u6570\uFF0C\
  \u5305\u62EC\u7A7A\u683C\u548C\u7279\u6B8A\u5B57\u7B26\u3002\u8FD9\u91CC\u6709\u4E00\
  \u4E9B\u793A\u4F8B\uFF1A."
lastmod: '2024-03-13T22:44:47.188574-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Google Apps \u811A\u672C\u4E2D\uFF0C\u60A8\u53EF\u4EE5\u4F7F\u7528\
  \ `.length` \u5C5E\u6027\u627E\u5230\u4E00\u4E2A\u5B57\u7B26\u4E32\u7684\u957F\u5EA6\
  \uFF0C\u8FD9\u4E0E JavaScript \u76F8\u4F3C\u3002\u6B64\u5C5E\u6027\u8FD4\u56DE\u5B57\
  \u7B26\u4E32\u5185\u7684\u5B57\u7B26\u6570\uFF0C\u5305\u62EC\u7A7A\u683C\u548C\u7279\
  \u6B8A\u5B57\u7B26\u3002\u8FD9\u91CC\u6709\u4E00\u4E9B\u793A\u4F8B\uFF1A."
title: "\u5BFB\u627E\u5B57\u7B26\u4E32\u7684\u957F\u5EA6"
weight: 7
---

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

---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:56:30.705237-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728\u57FA\u4E8EJavaScript\u7684Google\
  \ Apps Script\u4E2D\uFF0C\u60A8\u53EF\u4EE5\u4F7F\u7528`function`\u5173\u952E\u5B57\
  \u5B9A\u4E49\u51FD\u6570\uFF0C\u540E\u9762\u8DDF\u7740\u4E00\u4E2A\u552F\u4E00\u7684\
  \u51FD\u6570\u540D\u3001\u53EF\u4EE5\u5305\u542B\u53C2\u6570\u7684\u62EC\u53F7`()`\uFF0C\
  \u4EE5\u53CA\u5C01\u88C5\u51FD\u6570\u4EE3\u7801\u5757\u7684\u82B1\u62EC\u53F7`{}`\u3002\
  \u8FD9\u91CC\u6709\u4E00\u4E2A\u57FA\u672C\u793A\u4F8B\uFF1A."
lastmod: '2024-04-05T22:38:46.392209-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728\u57FA\u4E8EJavaScript\u7684Google\
  \ Apps Script\u4E2D\uFF0C\u60A8\u53EF\u4EE5\u4F7F\u7528`function`\u5173\u952E\u5B57\
  \u5B9A\u4E49\u51FD\u6570\uFF0C\u540E\u9762\u8DDF\u7740\u4E00\u4E2A\u552F\u4E00\u7684\
  \u51FD\u6570\u540D\u3001\u53EF\u4EE5\u5305\u542B\u53C2\u6570\u7684\u62EC\u53F7`()`\uFF0C\
  \u4EE5\u53CA\u5C01\u88C5\u51FD\u6570\u4EE3\u7801\u5757\u7684\u82B1\u62EC\u53F7`{}`\u3002\
  \u8FD9\u91CC\u6709\u4E00\u4E2A\u57FA\u672C\u793A\u4F8B\uFF1A."
title: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570"
weight: 18
---

## 如何操作：
在基于JavaScript的Google Apps Script中，您可以使用`function`关键字定义函数，后面跟着一个唯一的函数名、可以包含参数的括号`()`，以及封装函数代码块的花括号`{}`。这里有一个基本示例：

```javascript
function greetUser() {
  var user = Session.getActiveUser().getEmail();
  Logger.log('Hello, ' + user + '!');
}

greetUser();
```

示例输出：

```
Hello, someone@example.com!
```

现在，让我们考虑一个更实际的例子，与 Google Sheets 相关，我们将功能分成两个函数：一个用于设置工作表，另一个用于填充数据。

```javascript
function setupSheet() {
  var ss = SpreadsheetApp.getActiveSpreadsheet();
  var sheet = ss.getSheets()[0];
  sheet.setName('Sales Data');
  sheet.appendRow(['Item', 'Quantity', 'Price']);
}

function populateSheet(data) {
  var sheet = SpreadsheetApp.getActiveSpreadsheet().getSheetByName('Sales Data');
  data.forEach(function(row) {
    sheet.appendRow(row);
  });
}

// 初始化数据数组
var salesData = [
  ['Widgets', 15, 2.5],
  ['Gadgets', 8, 3.75]
];

// 运行函数
setupSheet();
populateSheet(salesData);
```

在这个示例中，`setupSheet` 准备工作表，而 `populateSheet` 则使用销售数据数组来填充工作表。分开这些关注点使得代码更加清晰，更容易适应变化。

## 深入探讨
将代码分成函数的概念，在 Google Apps Script 中并不新鲜或独特；它是几乎所有编程语言中倡导的基本编程实践。从历史上看，函数从数学概念的输入到输出的映射演变而来，这成为结构化编程的基石。这种方法促进了模块化和代码复用，为测试脚本的各个部分提供了清晰的途径。

Google Apps Script 基于 JavaScript，极大地受益于 JavaScript 的一等函数，允许将函数作为参数传递、从其他函数返回以及赋值给变量。这一特性开启了高级模式，如回调和函数式编程，尽管这些模式可能引入一些对 Google Apps Script 简单自动化任务而言不必要的复杂性。

对于更大的项目或更复杂的应用，开发者可能会探索使用 JavaScript 的新特性，如箭头函数、async/await 用于异步操作，甚至是 TypeScript 用于静态类型检查。特别是 TypeScript，可以编译为 Google Apps Script 运行，为寻求更强大类型检查和高级面向对象特性的开发者提供了一条途径。

然而，对于大多数 Google Apps 套件内的脚本需求，如演示的坚持使用简单、组织良好的函数提供了坚实的基础。这总是在利用高级功能提高效率和保持简单易维护、易读性之间的平衡行为。

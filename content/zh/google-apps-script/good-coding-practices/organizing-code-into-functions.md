---
title:                "将代码组织成函数"
aliases: - /zh/google-apps-script/organizing-code-into-functions.md
date:                  2024-02-01T21:56:30.705237-07:00
model:                 gpt-4-0125-preview
simple_title:         "将代码组织成函数"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/google-apps-script/organizing-code-into-functions.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

将代码组织成函数，是指通过将您的Google Apps Script代码分割成独立的逻辑段落，每个段落执行一个特定任务，从而对代码进行结构化。程序员这样做是为了提高代码的可读性、可维护性和可重用性，确保复杂的脚本更容易理解和调试。

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

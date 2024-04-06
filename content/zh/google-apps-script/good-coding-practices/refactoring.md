---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:59:59.558214-07:00
description: "\u5982\u4F55\u505A\uFF1A \u5728 Google Apps \u811A\u672C\u4E2D\uFF0C\
  \u53D7\u76CA\u4E8E\u91CD\u6784\u7684\u4E00\u4E2A\u5E38\u89C1\u60C5\u666F\u662F\u7B80\
  \u5316\u4E0E Google \u8868\u683C\u6216\u6587\u6863\u4EA4\u4E92\u7684\u7E41\u7410\
  \u811A\u672C\u3002\u6700\u521D\uFF0C\u811A\u672C\u53EF\u80FD\u4EE5\u4E00\u79CD\u5FEB\
  \u901F\u5E76\u80AE\u810F\u7684\u65B9\u5F0F\u7F16\u5199\uFF0C\u4EE5\u5FEB\u901F\u83B7\
  \u5F97\u7ED3\u679C\u3002\u968F\u7740\u65F6\u95F4\u7684\u63A8\u79FB\uFF0C\u968F\u7740\
  \u811A\u672C\u7684\u589E\u957F\uFF0C\u5B83\u53D8\u5F97\u96BE\u4EE5\u5904\u7406\u3002\
  \u8BA9\u6211\u4EEC\u901A\u8FC7\u4E00\u4E2A\u4F8B\u5B50\u6765\u770B\u770B\u5982\u4F55\
  \u91CD\u6784\u4EE5\u63D0\u9AD8\u53EF\u8BFB\u6027\u548C\u6548\u7387\u3002 **\u539F\
  \u59CB\u811A\u672C\uFF1A**."
lastmod: '2024-04-05T21:53:47.570313-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u91CD\u6784"
weight: 19
---

## 如何做：
在 Google Apps 脚本中，受益于重构的一个常见情景是简化与 Google 表格或文档交互的繁琐脚本。最初，脚本可能以一种快速并肮脏的方式编写，以快速获得结果。随着时间的推移，随着脚本的增长，它变得难以处理。让我们通过一个例子来看看如何重构以提高可读性和效率。

**原始脚本：**

```javascript
function logSheetNames() {
  var sheets = SpreadsheetApp.getActiveSpreadsheet().getSheets();
  for (var i = 0; i < sheets.length; i++) {
    Logger.log(sheets[i].getName());
  }
}
```

这个函数记录了 Google Spreadsheet 中每个表的名称。虽然它能正常工作，但它使用了过时的 JavaScript 实践，并且缺乏清晰度。

**重构后的脚本：**

```javascript
function logSheetNames() {
  const sheets = SpreadsheetApp.getActiveSpreadsheet().getSheets();
  sheets.forEach(sheet => Logger.log(sheet.getName()));
}
```

在重构版本中，我们切换到使用 `const` 来声明不会改变的变量，使我们的意图更明确。我们还使用了 `forEach` 方法，这是一种更现代和更简洁的数组迭代方法，提高了可读性。

**示例输出（两个脚本均适用）：**

假设您的 Google 表格文档有两个名为“Expenses”和“Revenue”的表格，日志的输出将类似于以下内容：

```
[20-04-2023 10:00:00: INFO] Expenses
[20-04-2023 10:00:01: INFO] Revenue
```

重构后的脚本实现了相同的结果，但更加干净，易于一目了然地理解。

## 深入解析
Google Apps 脚本中的重构部分地继承了更广泛软件工程实践的原则。它在20世纪90年代末变得更加被认可和结构化，特别是由于Martin Fowler的开创性著作《重构：改善既有代码的设计》（1999）的贡献，该书提供了各种重构技术的全面指南。虽然重构的具体细节可能因编程语言的语法和功能差异而异，但核心目标保持不变：改善代码而不改变其外部行为。

在 Google Apps 脚本的背景下，重构过程中需要考虑的一个关键方面是 Google 强加的服务配额和限制。高效地重构的代码不仅读起来更好，而且在这些限制范围内运行得更快、更可靠。例如，批量操作（使用 `Range.setValues()` 而不是一次设置一个单元格的值）可以显著减少执行时间和配额消耗。

然而，重要的是要注意，对于某些复杂项目，由于这些限制，Google Apps 脚本可能不足够用。在这些情况下，寻找替代方案，如 Google Cloud Functions 或 Apps 脚本的更新兄弟，AppSheet，可能会提供更好的可扩展性和功能。

最终，虽然重构是维护和改进 Google Apps 脚本项目的关键技能，理解环境的限制并考虑替代解决方案对于交付高效、健壮和可维护的代码同样重要。

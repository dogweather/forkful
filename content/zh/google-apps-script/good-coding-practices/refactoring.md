---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:59:59.558214-07:00
description: "\u5728\u7F16\u7A0B\u8BCD\u6C47\u4E2D\uFF0C\u91CD\u6784\u6307\u7684\u662F\
  \u91CD\u65B0\u7EC4\u7EC7\u73B0\u6709\u8BA1\u7B97\u673A\u4EE3\u7801\u7684\u8FC7\u7A0B\
  \u2014\u5728\u4E0D\u6539\u53D8\u5176\u5916\u90E8\u884C\u4E3A\u7684\u60C5\u51B5\u4E0B\
  \u66F4\u6539\u6784\u9020\u2014\u4EE5\u6539\u5584\u975E\u529F\u80FD\u5C5E\u6027\u3002\
  \u8FD9\u5BF9\u4E8E\u7A0B\u5E8F\u5458\u6765\u8BF4\u662F\u63D0\u9AD8\u4EE3\u7801\u53EF\
  \u8BFB\u6027\u3001\u964D\u4F4E\u590D\u6742\u5EA6\u3001\u4EE5\u53CA\u53EF\u80FD\u63ED\
  \u793A\u6F5C\u5728\u9519\u8BEF\u7684\u91CD\u8981\u6B65\u9AA4\uFF0C\u4ECE\u800C\u4FC3\
  \u8FDB\u66F4\u5BB9\u6613\u7684\u7EF4\u62A4\u548C\u672A\u6765\u4EE3\u7801\u7684\u53EF\
  \u6269\u5C55\u6027\u3002"
lastmod: 2024-02-19 22:05:06.290411
model: gpt-4-0125-preview
summary: "\u5728\u7F16\u7A0B\u8BCD\u6C47\u4E2D\uFF0C\u91CD\u6784\u6307\u7684\u662F\
  \u91CD\u65B0\u7EC4\u7EC7\u73B0\u6709\u8BA1\u7B97\u673A\u4EE3\u7801\u7684\u8FC7\u7A0B\
  \u2014\u5728\u4E0D\u6539\u53D8\u5176\u5916\u90E8\u884C\u4E3A\u7684\u60C5\u51B5\u4E0B\
  \u66F4\u6539\u6784\u9020\u2014\u4EE5\u6539\u5584\u975E\u529F\u80FD\u5C5E\u6027\u3002\
  \u8FD9\u5BF9\u4E8E\u7A0B\u5E8F\u5458\u6765\u8BF4\u662F\u63D0\u9AD8\u4EE3\u7801\u53EF\
  \u8BFB\u6027\u3001\u964D\u4F4E\u590D\u6742\u5EA6\u3001\u4EE5\u53CA\u53EF\u80FD\u63ED\
  \u793A\u6F5C\u5728\u9519\u8BEF\u7684\u91CD\u8981\u6B65\u9AA4\uFF0C\u4ECE\u800C\u4FC3\
  \u8FDB\u66F4\u5BB9\u6613\u7684\u7EF4\u62A4\u548C\u672A\u6765\u4EE3\u7801\u7684\u53EF\
  \u6269\u5C55\u6027\u3002"
title: "\u91CD\u6784"
---

{{< edit_this_page >}}

## 什么是重构及其原因？

在编程词汇中，重构指的是重新组织现有计算机代码的过程—在不改变其外部行为的情况下更改构造—以改善非功能属性。这对于程序员来说是提高代码可读性、降低复杂度、以及可能揭示潜在错误的重要步骤，从而促进更容易的维护和未来代码的可扩展性。

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

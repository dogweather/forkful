---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:58.212416-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Google Apps \u811A\u672C\u57FA\u4E8E\
  \ JavaScript\uFF0C\u4F7F\u6211\u4EEC\u80FD\u591F\u4F7F\u7528\u4F20\u7EDF\u7684 `try-catch`\
  \ \u8BED\u53E5\u8FDB\u884C\u9519\u8BEF\u5904\u7406\uFF0C\u5982\u679C\u9700\u8981\
  \u65E0\u8BBA\u6210\u529F\u8FD8\u662F\u51FA\u9519\u90FD\u8FDB\u884C\u6E05\u7406\uFF0C\
  \u5219\u8FD8\u53EF\u4EE5\u4F7F\u7528 `finally`\u3002"
lastmod: '2024-04-05T22:38:46.394707-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A Google Apps \u811A\u672C\u57FA\u4E8E JavaScript\uFF0C\
  \u4F7F\u6211\u4EEC\u80FD\u591F\u4F7F\u7528\u4F20\u7EDF\u7684 `try-catch` \u8BED\u53E5\
  \u8FDB\u884C\u9519\u8BEF\u5904\u7406\uFF0C\u5982\u679C\u9700\u8981\u65E0\u8BBA\u6210\
  \u529F\u8FD8\u662F\u51FA\u9519\u90FD\u8FDB\u884C\u6E05\u7406\uFF0C\u5219\u8FD8\u53EF\
  \u4EE5\u4F7F\u7528 `finally`\u3002"
title: "\u5904\u7406\u9519\u8BEF"
weight: 16
---

## 如何操作：
Google Apps 脚本基于 JavaScript，使我们能够使用传统的 `try-catch` 语句进行错误处理，如果需要无论成功还是出错都进行清理，则还可以使用 `finally`。

```javascript
function myFunction() {
  try {
    // 可能抛出错误的代码
    var sheet = SpreadsheetApp.getActiveSheet();
    var data = sheet.getRange("A1").getValue();
    if (data === "") {
      throw new Error("A1 单元格为空。");
    }
    Logger.log(data);
  } catch (e) {
    // 错误处理代码
    Logger.log("错误：" + e.message);
  } finally {
    // 清理代码，无论是否发生错误都会执行
    Logger.log("函数完成。");
  }
}
```

无错误的示例输出：
```
[单元格值]
函数完成。
```

带有错误的示例输出（假设 A1 为空）：
```
错误：A1 单元格为空。
函数完成。
```

Google Apps 脚本还支持使用 `Error` 对象抛出自定义错误，并在需要时捕获特定错误类型。然而，缺乏高级错误分类使得依赖错误消息以获得特定性变得极其重要。

## 深入探讨
从历史上看，像 JavaScript（以及扩展到 Google Apps 脚本）这样的脚本语言的错误处理比某些编译语言要不那么复杂，编译语言提供了如详细的异常层次结构和全面的调试工具等特性。Google Apps 脚本的模型相对简单，利用 JavaScript 的 `try-catch-finally` 范式。这种简单性与该语言的设计相一致，即在 Google 的生态系统内快速开发和部署小到中等规模的应用程序，但它有时会限制处理复杂错误场景的开发人员。

在更复杂的应用程序中，程序员经常用自定义日志记录和错误报告机制补充 Google Apps 脚本的本地错误处理。这可能包括将错误写入 Google 表格进行审计，或使用 Google Apps Script 的 URL Fetch Services 通过第三方日志记录服务将错误详细信息发送出脚本环境。

尽管 Google Apps 脚本在内置错误处理的复杂性和能力方面可能落后于 Java 或 C# 等语言，但其与 Google 服务的集成以及 `try-catch-finally` 方法的简单性使它成为开发人员快速自动化任务和创建 Google 生态系统内集成的强大工具。其他背景的开发者可能会发现，挑战不在于掌握复杂的错误处理模式，而在于创造性地利用现有资源，确保他们的脚本是强大而用户友好的。

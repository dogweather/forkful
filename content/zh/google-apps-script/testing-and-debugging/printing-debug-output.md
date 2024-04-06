---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:58.437501-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Google Apps Script\u63D0\u4F9B\u4E86\
  `Logger`\u7C7B\u7528\u4E8E\u57FA\u672C\u8C03\u8BD5\uFF0C\u5BF9\u4E8E\u66F4\u9AD8\
  \u7EA7\u7684\u9700\u6C42\uFF0CV8\u8FD0\u884C\u65F6\u5F15\u5165\u4E86`console`\u7C7B\
  \u3002 **\u4F7F\u7528Logger\uFF1A** Logger\u7C7B\u5141\u8BB8\u60A8\u8BB0\u5F55\u8C03\
  \u8BD5\u6D88\u606F\uFF0C\u60A8\u53EF\u4EE5\u5728\u6267\u884C\u540E\u5728Apps Script\u7F16\
  \u8F91\u5668\u4E2D\u901A\u8FC7`\u67E5\u770B >\u2026"
lastmod: '2024-04-05T21:53:47.561817-06:00'
model: gpt-4-0125-preview
summary: "**\u4F7F\u7528Logger\uFF1A** Logger\u7C7B\u5141\u8BB8\u60A8\u8BB0\u5F55\u8C03\
  \u8BD5\u6D88\u606F\uFF0C\u60A8\u53EF\u4EE5\u5728\u6267\u884C\u540E\u5728Apps Script\u7F16\
  \u8F91\u5668\u4E2D\u901A\u8FC7`\u67E5\u770B > \u65E5\u5FD7`\u67E5\u770B\u8FD9\u4E9B\
  \u6D88\u606F\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\u7B80\u5355\u7684\u4F8B\u5B50\uFF1A\
  ."
title: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA"
weight: 33
---

## 如何操作：
Google Apps Script提供了`Logger`类用于基本调试，对于更高级的需求，V8运行时引入了`console`类。

**使用Logger：**

Logger类允许您记录调试消息，您可以在执行后在Apps Script编辑器中通过`查看 > 日志`查看这些消息。这里有一个简单的例子：

```javascript
function logSample() {
  var name = "Wired Reader";
  Logger.log("Hello, %s!", name);
}
```

运行`logSample()`后，您可以在日志查看器中查看带有"Hello, Wired Reader!"的日志。

**在V8运行时使用console.log：**

通过V8运行时，`console.log`为来自其他语言的开发者提供了更熟悉的语法：

```javascript
function consoleSample() {
  var status = 'active';
  var count = 150;
  console.log(`Current status: ${status}, Count: ${count}`);
}
```

执行后，访问`查看 > Stackdriver日志`以查看输出。它更强大，支持字符串插值和对象检查，并与Google Cloud的日志记录集成，提供持久日志和高级过滤功能。

**来自console.log的样本输出：**

```
Current status: active, Count: 150
```

## 深入了解
起初，`Logger.log`是Google Apps Script中用于调试的主要工具，提供了一种简单直接的方法来打印输出以供检查。然而，随着脚本变得越来越复杂并与Google Cloud Platform服务集成，对更健壮的日志解决方案的需求变得显而易见。

随着V8运行时的引入，`console.log`被纳入其中。这不仅使Google Apps Script与标准JavaScript语法保持一致，使熟悉JavaScript的开发者更易接近该语言，而且还利用了Google Cloud日志记录功能的强大基础设施。`console.log`及其与Google Cloud Platform的集成标志着Google Apps Script调试能力的显著进化，为开发者提供了一种更为动态和可扩展的方法来监控和故障排除他们的脚本。

虽然`Logger.log`足以满足基本调试需求和小型项目，但V8运行时的`console.log`提供了更全面和未来证明的解决方案。这包括在执行会话之外保留日志的能力、在Google Cloud控制台中搜索和过滤日志的能力，以及与现代JavaScript开发实践的整体一致性。然而，开发者应根据他们的项目的复杂性和规模来衡量他们的需求，以在这些选项之间做出选择。

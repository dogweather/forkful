---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:58.437501-07:00
description: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA\u6D89\u53CA\u5728\u4EE3\u7801\u4E2D\
  \u7B56\u7565\u6027\u5730\u653E\u7F6E\u65E5\u5FD7\u58F0\u660E\uFF0C\u4EE5\u4FBF\u5728\
  \u8FD0\u884C\u65F6\u663E\u793A\u53D8\u91CF\u503C\u3001\u6267\u884C\u6D41\u7A0B\u6216\
  \u6D88\u606F\u9519\u8BEF\u3002\u7A0B\u5E8F\u5458\u5E7F\u6CDB\u5229\u7528\u5B83\u6765\
  \u8DDF\u8E2A\u548C\u8BCA\u65AD\u4ED6\u4EEC\u7684\u811A\u672C\u884C\u4E3A\uFF0C\u786E\
  \u4FDD\u4ED6\u4EEC\u7684Google Apps Script\u5E94\u7528\u7A0B\u5E8F\u7684\u6B63\u786E\
  \u6027\u548C\u6548\u7387\u3002"
lastmod: '2024-03-13T22:44:47.205046-06:00'
model: gpt-4-0125-preview
summary: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA\u6D89\u53CA\u5728\u4EE3\u7801\u4E2D\
  \u7B56\u7565\u6027\u5730\u653E\u7F6E\u65E5\u5FD7\u58F0\u660E\uFF0C\u4EE5\u4FBF\u5728\
  \u8FD0\u884C\u65F6\u663E\u793A\u53D8\u91CF\u503C\u3001\u6267\u884C\u6D41\u7A0B\u6216\
  \u6D88\u606F\u9519\u8BEF\u3002\u7A0B\u5E8F\u5458\u5E7F\u6CDB\u5229\u7528\u5B83\u6765\
  \u8DDF\u8E2A\u548C\u8BCA\u65AD\u4ED6\u4EEC\u7684\u811A\u672C\u884C\u4E3A\uFF0C\u786E\
  \u4FDD\u4ED6\u4EEC\u7684Google Apps Script\u5E94\u7528\u7A0B\u5E8F\u7684\u6B63\u786E\
  \u6027\u548C\u6548\u7387\u3002."
title: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA"
weight: 33
---

## 什么和为什么？

打印调试输出涉及在代码中策略性地放置日志声明，以便在运行时显示变量值、执行流程或消息错误。程序员广泛利用它来跟踪和诊断他们的脚本行为，确保他们的Google Apps Script应用程序的正确性和效率。

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

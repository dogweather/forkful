---
title:                "打印调试输出"
aliases: - /zh/google-apps-script/printing-debug-output.md
date:                  2024-02-01T21:57:58.437501-07:00
model:                 gpt-4-0125-preview
simple_title:         "打印调试输出"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/google-apps-script/printing-debug-output.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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

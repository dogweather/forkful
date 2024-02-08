---
title:                "写入标准错误"
aliases:
- zh/google-apps-script/writing-to-standard-error.md
date:                  2024-02-01T22:09:09.344277-07:00
model:                 gpt-4-0125-preview
simple_title:         "写入标准错误"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/google-apps-script/writing-to-standard-error.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么和为什么？

在编程语言中写入标准错误(stderr)是关于将错误消息和诊断信息引导到一个单独的流中，而不是标准输出(stdout)。程序员这样做是为了将正常程序输出与错误消息区分开来，使得调试和日志分析更加直接。

## 如何：

Google Apps Script 作为一个在 Google Apps 平台上进行轻量级应用开发的脚本语言，并不像你在 Node.js 或 Python 中找到的那样提供一个直接内置的函数，例如 `console.error()` 来写入 stderr。然而，你可以通过使用 Google Apps Script 的日志服务或自定义错误处理来模拟这种行为，以管理和隔离错误输出。

### 示例：使用 `Logger` 记录错误消息

```javascript
function logError() {
  try {
    // 模拟一个错误
    const result = 1 / 0;
    if(!isFinite(result)) throw new Error("尝试除以零");
  } catch (e) {
    // 将错误消息写入日志
    Logger.log('错误: ' + e.message);
  }
}
```

当你运行 `logError()` 时，这将把错误消息写入 Google Apps Script 的日志中，你可以通过 `查看 > 日志` 查看。这并不完全是 stderr，但它起到了类似的作用，将错误日志与标准输出分开。

### 高级诊断日志

对于更高级的调试和错误日志记录，你可以使用 Stackdriver 日志记录，现在称为 Google Cloud 的操作套件。

```javascript
function advancedErrorLogging() {
  try {
    // 故意引起一个错误
    const obj = null;
    const result = obj.someProperty;
  } catch (e) {
    console.error('遇到错误: ', e.toString());
  }
}
```

这将把错误消息引导到 Stackdriver 日志记录中，在那里它被管理为一个错误级别的日志。注意，Stackdriver/Google Cloud的操作套件集成提供了一个比 `Logger` 更细粒度且可搜索的日志解决方案。

## 深入研究

Google Apps Script 缺乏专用的 `stderr` 流反映了其作为基于云的脚本语言的性质和起源，其中传统的控制台或终端基础输出(如 stdout 和 stderr)不那么相关。从历史上看，Google Apps Script 设计用于通过简单脚本增强 Google Apps 的功能，重点是易用性而非在更复杂的编程环境中可用的综合性特性。

尽管如此，Google Apps Script 向更复杂的应用开发进化促使开发人员采用创造性的方法来处理错误和日志记录，利用像 Logger 这样的现有服务并与 Google Cloud 的操作套件集成。这些方法，虽然不是直接的 stderr 实现，但为错误管理和诊断日志记录提供了强大的替代方案，在以云为中心的环境中。

关键是，虽然这些方法在 Google Apps Script 的生态系统内起到了作用，但它们强调了平台与传统编程环境相比的限制。对于需要详细和分层错误处理策略的开发人员，整合外部日志服务或采用 Google Cloud 函数，后者提供了更传统的 stderr 和 stdout 处理方式，可能更可取。

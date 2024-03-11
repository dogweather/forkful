---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:40.605609-07:00
description: "\u5728 Google Apps Script (GAS) \u4E2D\u8FDB\u884C\u8C03\u8BD5\u6D89\
  \u53CA\u5230\u8BC6\u522B\u548C\u79FB\u9664\u65E8\u5728\u81EA\u52A8\u5316 Google\
  \ \u5E94\u7528\u6216\u6784\u5EFA\u7F51\u7EDC\u5E94\u7528\u7684\u811A\u672C\u4E2D\
  \u7684\u9519\u8BEF\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\u8FDB\u884C\u8C03\u8BD5\
  \u4EE5\u786E\u4FDD\u4ED6\u4EEC\u7684\u4EE3\u7801\u5982\u9884\u671F\u90A3\u6837\u6267\
  \u884C\uFF0C\u4ECE\u800C\u63D0\u5347\u5E94\u7528\u7684\u53EF\u9760\u6027\u548C\u6027\
  \u80FD\u3002"
lastmod: '2024-03-11T00:14:20.970433-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Google Apps Script (GAS) \u4E2D\u8FDB\u884C\u8C03\u8BD5\u6D89\u53CA\
  \u5230\u8BC6\u522B\u548C\u79FB\u9664\u65E8\u5728\u81EA\u52A8\u5316 Google \u5E94\
  \u7528\u6216\u6784\u5EFA\u7F51\u7EDC\u5E94\u7528\u7684\u811A\u672C\u4E2D\u7684\u9519\
  \u8BEF\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\u8FDB\u884C\u8C03\u8BD5\u4EE5\u786E\
  \u4FDD\u4ED6\u4EEC\u7684\u4EE3\u7801\u5982\u9884\u671F\u90A3\u6837\u6267\u884C\uFF0C\
  \u4ECE\u800C\u63D0\u5347\u5E94\u7528\u7684\u53EF\u9760\u6027\u548C\u6027\u80FD\u3002"
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 Google Apps Script (GAS) 中进行调试涉及到识别和移除旨在自动化 Google 应用或构建网络应用的脚本中的错误的过程。程序员进行调试以确保他们的代码如预期那样执行，从而提升应用的可靠性和性能。

## 如何操作：

Google Apps Script 提供了内置的调试器，位于 Apps Script 编辑器中，以帮助排查脚本问题。以下是启动和使用调试器的方法：

1. **在 Apps Script 编辑器中打开你的脚本。**
2. **选择要调试的函数。**从顶部的下拉菜单中选择你希望调试的函数。
3. **设置断点。**在你希望暂停执行的行号左侧的灰色区域（代码装订线）点击；一个红点出现，表示一个断点。
4. **开始调试。**点击bug图标或选择`调试` > `开始调试`。执行将开始并在第一个断点处暂停。

考虑这个简单的脚本：

```javascript
function calculateSum() {
  var a = 5;
  var b = 10;
  var sum = a + b;
  Logger.log(sum); // 旨在记录 15
}
```

如果不确定为什么`Logger.log(sum)`没有显示预期的结果，你可以在`var sum = a + b;`这一行设置一个断点，并逐行检查脚本来审查变量值。

**在 Logger 中的示例输出：**

```plain
15
```

在调试时，Apps Script 编辑器允许你：

- **逐步执行代码**，使用跨过、跳入和跳出按钮。
- **观察表达式和变量**，实时查看它们的值变化。
- **检查调用堆栈**，追踪函数调用。

## 深入了解

在 Google Apps Script 中进行调试，如同在任何其他编程环境中一样，对于创建无错误的应用至关重要。GAS 的内置调试器自开发初期就引入，提供了基本能力来逐步检查和修复代码。尽管它提供了基本的调试特性，类似于在更成熟的环境（如 Visual Studio Code 或 IntelliJ）中找到的那些特性，但对于复杂的调试场景来说，它可能力不从心。例如，其检查异步回调或管理重型脚本执行的能力可能会受限。

对于复杂的调试需求，开发者可能会采取替代方法，例如大量的日志记录（使用`Logger.log()`）或甚至部署为网络应用以检查真实世界场景中的行为。然而，GAS 调试器的简单性及其在 Apps Script 编辑器中的集成，使其成为排除故障和理解脚本行为的宝贵第一步。值得注意的是，随着 Google 对 Apps Script 的持续更新和增强，调试体验正在稳步改善，随时间提供更多复杂的工具和选项。这种演化反映了 Google 让 Apps Script 成为一个对来自多元背景的开发者更加强大和易于访问的平台的承诺。

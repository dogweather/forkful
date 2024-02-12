---
title:                "计算未来或过去的日期"
aliases:
- zh/google-apps-script/calculating-a-date-in-the-future-or-past.md
date:                  2024-02-01T21:48:51.690516-07:00
model:                 gpt-4-0125-preview
simple_title:         "计算未来或过去的日期"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/google-apps-script/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

计算将来或过去的日期就是对日期对象进行操纵，以找到当前日期之后或之前的日期。程序员进行此操作的任务范围很广，从设置提醒和过期日期到分析基于时间的数据趋势。

## 如何操作：

在基于JavaScript的Google Apps Script中，您可以使用`Date`对象来操纵日期。下面是如何计算将来和过去的日期：

### 计算将来的日期

要计算将来的日期，您需要为当前日期创建一个日期对象，然后向其添加所需的天数（或任何其他时间单位）。

```javascript
// 当前日期
var today = new Date();

// 计算将来10天的日期
var futureDate = new Date(today);
futureDate.setDate(today.getDate() + 10);

Logger.log("将来的日期: " + futureDate.toDateString());
```

### 计算过去的日期

类似地，要找到过去的某个日期，从当前日期减去天数。

```javascript
// 当前日期
var today = new Date();

// 计算过去10天的日期
var pastDate = new Date(today);
pastDate.setDate(today.getDate() - 10);

Logger.log("过去的日期: " + pastDate.toDateString());
```

### 样本输出

这将输出类似以下内容（假设今天是2023年4月15日）：

```
将来的日期: Tue Apr 25 2023
过去的日期: Wed Apr 05 2023
```

请记住，JavaScript（因此在Google Apps Script中的）的Date对象在您增加或减少天数时自动调整月份和年份。

## 深入了解

使用`Date`对象进行日期操控源于早期JavaScript实现。随着时间的推移，这种方法总体上保持一致，为开发者提供了一种直观的方式来管理日期，无需外部库。然而，对于更复杂的操作，如时区调整，或在处理大量基于日期的数据时，像`Moment.js`或更现代的`Luxon`等库可能提供更多功能和更容易的处理方法。

特别是在Google Apps Script中，尽管`Date`对象直接可用且简单，但要注意日期计算如何影响脚本性能和执行时间，特别是在时间驱动的触发器或大量的电子表格操控中。此外，尽管Google Apps Script提供了内置方法在其生态系统内处理日期（如在Google Sheets或Calendar中），但在复杂情形下，集成外部库或利用Google的高级服务有时可以提供更强大的解决方案。

因此，虽然原生JavaScript的`Date`对象方法通常足以应付简单的计算，但探索外部库或服务可以增强功能，以满足更细致的要求。

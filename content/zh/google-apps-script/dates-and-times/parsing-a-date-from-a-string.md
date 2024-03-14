---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:20.527198-07:00
description: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F\u6D89\u53CA\u5C06\u4EE3\
  \u8868\u65E5\u671F\u7684\u6587\u672C\u8F6C\u6362\u4E3A\u65E5\u671F\u5BF9\u8C61\uFF0C\
  \u4F7F\u7A0B\u5E8F\u5458\u80FD\u591F\u6267\u884C\u4E0E\u65E5\u671F\u76F8\u5173\u7684\
  \u64CD\u4F5C\uFF0C\u5982\u6BD4\u8F83\u3001\u7B97\u672F\u548C\u683C\u5F0F\u5316\u3002\
  \u8FD9\u5BF9\u4E8E\u5904\u7406\u7528\u6237\u8F93\u5165\u3001\u5904\u7406\u5916\u90E8\
  \u6765\u6E90\u7684\u6570\u636E\u4EE5\u53CA\u7BA1\u7406\u5404\u79CD\u683C\u5F0F\u7684\
  \u65E5\u671F\u81F3\u5173\u91CD\u8981\uFF0C\u7279\u522B\u662F\u5728\u6D89\u53CA\u65E5\
  \u7A0B\u5B89\u6392\u3001\u6570\u636E\u5206\u6790\u6216\u4EFB\u4F55\u5F62\u5F0F\u7684\
  \u57FA\u4E8E\u65F6\u95F4\u7684\u8BB0\u5F55\u7684\u5E94\u7528\u7A0B\u5E8F\u4E2D\u3002"
lastmod: '2024-03-13T22:44:47.214692-06:00'
model: gpt-4-0125-preview
summary: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F\u6D89\u53CA\u5C06\u4EE3\
  \u8868\u65E5\u671F\u7684\u6587\u672C\u8F6C\u6362\u4E3A\u65E5\u671F\u5BF9\u8C61\uFF0C\
  \u4F7F\u7A0B\u5E8F\u5458\u80FD\u591F\u6267\u884C\u4E0E\u65E5\u671F\u76F8\u5173\u7684\
  \u64CD\u4F5C\uFF0C\u5982\u6BD4\u8F83\u3001\u7B97\u672F\u548C\u683C\u5F0F\u5316\u3002\
  \u8FD9\u5BF9\u4E8E\u5904\u7406\u7528\u6237\u8F93\u5165\u3001\u5904\u7406\u5916\u90E8\
  \u6765\u6E90\u7684\u6570\u636E\u4EE5\u53CA\u7BA1\u7406\u5404\u79CD\u683C\u5F0F\u7684\
  \u65E5\u671F\u81F3\u5173\u91CD\u8981\uFF0C\u7279\u522B\u662F\u5728\u6D89\u53CA\u65E5\
  \u7A0B\u5B89\u6392\u3001\u6570\u636E\u5206\u6790\u6216\u4EFB\u4F55\u5F62\u5F0F\u7684\
  \u57FA\u4E8E\u65F6\u95F4\u7684\u8BB0\u5F55\u7684\u5E94\u7528\u7A0B\u5E8F\u4E2D\u3002"
title: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u89E3\u6790\u65E5\u671F"
---

{{< edit_this_page >}}

## 什么 & 为什么？

从字符串解析日期涉及将代表日期的文本转换为日期对象，使程序员能够执行与日期相关的操作，如比较、算术和格式化。这对于处理用户输入、处理外部来源的数据以及管理各种格式的日期至关重要，特别是在涉及日程安排、数据分析或任何形式的基于时间的记录的应用程序中。

## 如何操作：

在基于 JavaScript 的 Google Apps Script 中，您有几种方法可以从字符串解析日期。以下是使用原生 JavaScript 方法和 Google Apps Script 实用程序的示例。

**使用 `new Date()` 构造器：**

在 Google Apps Script 中，将字符串解析为日期的最简单方法是使用 `Date` 对象的构造器。然而，它要求日期字符串必须是 Date.parse() 方法可识别的格式（例如，YYYY-MM-DD）。

```javascript
const dateString = '2023-04-01';
const dateObject = new Date(dateString);
Logger.log(dateObject); // 记录 Sat Apr 01 2023 00:00:00 GMT+0000 (UTC)
```

**使用 `Utilities.parseDate()`:**

为了更大的灵活性，特别是对自定义日期格式，Google Apps Script 提供了 `Utilities.parseDate()`。这个方法允许你指定日期格式、时区和地区设置。

```javascript
const dateString = '01-04-2023'; // DD-MM-YYYY
const format = 'dd-MM-yyyy';
const timezone = Session.getScriptTimeZone();
const dateObject = Utilities.parseDate(dateString, timezone, format);
Logger.log(dateObject); // 根据脚本的时区记录 Sat Apr 01 2023 00:00:00 GMT+0000 (UTC)
```

注意：虽然 `Utilities.parseDate()` 提供了更多控制，但其行为可能会根据脚本的时区而有所不同，因此如果您的应用程序处理多个地区的日期，显式指定时区非常关键。

## 深入探讨

编程语言中的日期解析历来充满挑战，主要是由于日期格式的多样性和时区的复杂性。Google Apps Script 的方法主要源自 JavaScript，旨在通过提供简单的 `Date` 对象和更多功能的 `Utilities.parseDate()` 函数来简化此问题。然而，每种方法都有其局限性；例如，依赖 `Date` 构造器处理字符串会导致不同环境下对日期格式的解释不一致。另一方面，`Utilities.parseDate()` 需要对格式、时区和地区设置有更清晰的理解，使其稍微复杂一些，但对特定需求更可靠。

备选的库或服务，如 Moment.js（现在推荐对新项目使用 Luxon），提供了更丰富的功能和更好的时区处理，解决了许多这些挑战。然而，在 Google Apps Script 的背景下，外部库的使用受到限制，有效地了解和利用内置方法变得至关重要。来自其他语言的程序员可能会发现 Google Apps Script 中日期处理的细微之处独特而具有挑战性，但通过深入了解可用工具并仔细考虑应用的全球性质，可以实现稳健的日期解析。

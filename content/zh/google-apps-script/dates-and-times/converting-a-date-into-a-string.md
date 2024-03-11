---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:51:02.388773-07:00
description: "\u5C06\u65E5\u671F\u8F6C\u6362\u6210\u5B57\u7B26\u4E32\u662F\u4E00\u4E2A\
  \u57FA\u7840\u4EFB\u52A1\uFF0C\u5B83\u4F7F\u7A0B\u5E8F\u5458\u80FD\u591F\u4EE5\u4EBA\
  \u7C7B\u53EF\u8BFB\u7684\u683C\u5F0F\u64CD\u4F5C\u548C\u663E\u793A\u65E5\u671F\u4FE1\
  \u606F\u3002\u8FD9\u5BF9\u4E8E\u521B\u5EFA\u7528\u6237\u754C\u9762\u3001\u751F\u6210\
  \u62A5\u544A\u6216\u5728\u4F7F\u7528Google Apps Script\u5F00\u53D1\u7684\u5E94\u7528\
  \u7A0B\u5E8F\u4E2D\u8BB0\u5F55\u4FE1\u606F\u81F3\u5173\u91CD\u8981\u3002"
lastmod: '2024-03-11T00:14:20.980590-06:00'
model: gpt-4-0125-preview
summary: "\u5C06\u65E5\u671F\u8F6C\u6362\u6210\u5B57\u7B26\u4E32\u662F\u4E00\u4E2A\
  \u57FA\u7840\u4EFB\u52A1\uFF0C\u5B83\u4F7F\u7A0B\u5E8F\u5458\u80FD\u591F\u4EE5\u4EBA\
  \u7C7B\u53EF\u8BFB\u7684\u683C\u5F0F\u64CD\u4F5C\u548C\u663E\u793A\u65E5\u671F\u4FE1\
  \u606F\u3002\u8FD9\u5BF9\u4E8E\u521B\u5EFA\u7528\u6237\u754C\u9762\u3001\u751F\u6210\
  \u62A5\u544A\u6216\u5728\u4F7F\u7528Google Apps Script\u5F00\u53D1\u7684\u5E94\u7528\
  \u7A0B\u5E8F\u4E2D\u8BB0\u5F55\u4FE1\u606F\u81F3\u5173\u91CD\u8981\u3002"
title: "\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\u4E32"
---

{{< edit_this_page >}}

## 何为及其原因？

将日期转换成字符串是一个基础任务，它使程序员能够以人类可读的格式操作和显示日期信息。这对于创建用户界面、生成报告或在使用Google Apps Script开发的应用程序中记录信息至关重要。

## 如何实现：

Google Apps Script基于JavaScript，提供了多种方法来实现日期到字符串的转换。以下是一些不同方法的示例：

### 使用`toString()`方法：
最直接的方法是使用`toString()`方法，它将日期对象转换为默认格式的字符串。

```javascript
var date = new Date();  // 创建一个新的日期对象
var dateString = date.toString();
Logger.log(dateString); // 输出："Wed Apr 05 2023 12:34:56 GMT-0700 (太平洋夏令时间)"
```

### 使用`toDateString()`方法：
为了只获取可读格式的日期部分而不包括时间信息，可以使用`toDateString()`。

```javascript
var date = new Date();
var dateString = date.toDateString();
Logger.log(dateString); // 输出："Wed Apr 05 2023"
```

### 使用`Utilities.formatDate()`进行自定义格式：
为了更精确地控制格式，Google Apps Script提供了`Utilities.formatDate()`。这个方法需要三个参数：日期对象、时区和格式字符串。

```javascript
var date = new Date();
var timeZone = Session.getScriptTimeZone();
var formattedDate = Utilities.formatDate(date, timeZone, "YYYY-MM-dd");
Logger.log(formattedDate); // 输出："2023-04-05"
```

这个方法对于生成特定于地区或特定于特定应用需求的日期格式特别有效。

## 深入探讨

将日期转换成字符串的需要并非仅限于Google Apps Script；它在所有编程语言中都很常见。然而，Google Apps Script继承自JavaScript的方法提供了一套灵活的选项，专门针对基于网页的脚本。`Utilities.formatDate()`通过认识到处理时区时的复杂性——一个经常被忽视的挑战，而脱颖而出。

从历史上看，处理日期和时间一直是软件开发中的一个错误和复杂性源泉，主要是由于时区和格式的差异。Google Apps Script中`Utilities.formatDate()`的引入，是向标准化日期时间操作致敬，特别是在Google全球使用的产品套件的背景下。

然而，当需要对时区、地区和格式有精确控制的时候，特别是在国际化应用中，开发人员可能会发现自己需要利用外部库，如`Moment.js`（尽管由于包大小的考量和现代化特性，它越来越倾向于使用`Luxon`、`Day.js`和`date-fns`）。这种方法自然会带来增加外部依赖和可能增加项目复杂性的权衡。

尽管有使用外部库的潜力，`Utilities.formatDate()`以及原生JavaScript日期方法为大多数常见用例提供了强大的解决方案。精明的开发人员将根据其项目的具体需求，平衡内置函数的简单性和便利性与外部库的强大性和灵活性。

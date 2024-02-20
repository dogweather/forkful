---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:38.327929-07:00
description: "\u5728 Google Apps \u811A\u672C\u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\
  \u662F\u5173\u4E8E\u83B7\u53D6\u5B9E\u65F6\u65E5\u671F\u548C\u65F6\u95F4\u7684\u5E38\
  \u89C1\u4EFB\u52A1\uFF0C\u7528\u4E8E\u81EA\u52A8\u5316\u4EFB\u52A1\u3001\u65E5\u5FD7\
  \u8BB0\u5F55\u548C\u5728 Google \u751F\u6001\u7CFB\u7EDF\u5185\u7684\u5E94\u7528\
  \u4E2D\u8FDB\u884C\u65F6\u95F4\u6233\u8BB0\u5F55\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\
  \u8FD9\u4E00\u529F\u80FD\u8FDB\u884C\u52A8\u6001\u5185\u5BB9\u751F\u6210\u3001\u622A\
  \u6B62\u65E5\u671F\u8DDF\u8E2A\u4EE5\u53CA\u5728 Google Docs\u3001Sheets \u548C\u5176\
  \u4ED6 Google \u670D\u52A1\u5185\u7684\u8C03\u5EA6\u3002"
lastmod: 2024-02-19 22:05:06.293557
model: gpt-4-0125-preview
summary: "\u5728 Google Apps \u811A\u672C\u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\
  \u662F\u5173\u4E8E\u83B7\u53D6\u5B9E\u65F6\u65E5\u671F\u548C\u65F6\u95F4\u7684\u5E38\
  \u89C1\u4EFB\u52A1\uFF0C\u7528\u4E8E\u81EA\u52A8\u5316\u4EFB\u52A1\u3001\u65E5\u5FD7\
  \u8BB0\u5F55\u548C\u5728 Google \u751F\u6001\u7CFB\u7EDF\u5185\u7684\u5E94\u7528\
  \u4E2D\u8FDB\u884C\u65F6\u95F4\u6233\u8BB0\u5F55\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\
  \u8FD9\u4E00\u529F\u80FD\u8FDB\u884C\u52A8\u6001\u5185\u5BB9\u751F\u6210\u3001\u622A\
  \u6B62\u65E5\u671F\u8DDF\u8E2A\u4EE5\u53CA\u5728 Google Docs\u3001Sheets \u548C\u5176\
  \u4ED6 Google \u670D\u52A1\u5185\u7684\u8C03\u5EA6\u3002"
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
---

{{< edit_this_page >}}

## 什么与为什么？

在 Google Apps 脚本中获取当前日期是关于获取实时日期和时间的常见任务，用于自动化任务、日志记录和在 Google 生态系统内的应用中进行时间戳记录。程序员使用这一功能进行动态内容生成、截止日期跟踪以及在 Google Docs、Sheets 和其他 Google 服务内的调度。

## 如何操作：

Google Apps 脚本是基于 JavaScript 的，提供了获取当前日期的简单方法。您可以使用 `new Date()` 构造函数来创建一个新的日期对象，代表当前的日期和时间。下面是如何以各种格式操作和显示这个日期的方法。

```javascript
function showCurrentDate() {
  var currentDate = new Date();
  
  Logger.log(currentDate); // 在脚本的时区内记录当前的日期和时间
  
  // 仅显示日期的 YYYY-MM-DD 格式
  var dateString = currentDate.getFullYear() + '-' + 
                   (currentDate.getMonth() + 1).toString().padStart(2, '0') + '-' + 
                   currentDate.getDate().toString().padStart(2, '0');
  Logger.log(dateString); // 示例输出："2023-04-01"
  
  // 以更易读的格式显示
  var options = { year: 'numeric', month: 'long', day: 'numeric', hour: '2-digit', minute: '2-digit', second: '2-digit', timeZoneName: 'short' };
  var readableDate = currentDate.toLocaleDateString('en-US', options) + ' ' + 
                     currentDate.toLocaleTimeString('en-US', options);
                     
  Logger.log(readableDate); // 示例输出："April 1, 2023, 12:00:00 PM GMT+1"
}
```

这些代码段演示了如何捕获和格式化当前的日期和时间，展示了在 Google Apps 脚本内各种编程需求的多功能性。

## 深入了解

在 JavaScript 确定使用 `Date` 对象之前，程序员不得不通过不够标准且更加繁琐的方式手动跟踪时间和日期。这包括使用时间戳整数和自制的日期函数，这些方法在不同的编程环境中有所不同，导致了一致性和兼容性问题。

JavaScript 引入的 `new Date()` 对象，以及 Google Apps 脚本的扩展，标准化了日期和时间操作，使它们更为直观，同时减少了进行日期相关操作所需的代码量。值得注意的是，尽管 Google Apps 脚本的实现方便且对 Google 的一系列产品内的许多应用足够用，但它可能不适用于所有场景，特别是那些需要复杂的时区处理或在快节奏环境中进行精确时间戳记录的场景。

对于这些高级用例，程序员常常转向诸如 Moment.js 或在 JavaScript 中的 date-fns 等库。虽然 Google Apps 脚本不支持这些库，但开发者可以使用可用的 JavaScript 日期方法模仿其中的一些功能，或通过 HTML 服务或 Apps 脚本的 URL Fetch 服务访问外部库。尽管有这些替代方案，Google Apps 脚本的原生日期和时间函数的简单性及其与 Google 生态系统任务的集成仍是大多数选择。

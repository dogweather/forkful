---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:47.787765-07:00
description: "\u5728JavaScript\u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u662F\u4E00\
  \u9879\u57FA\u672C\u4EFB\u52A1\uFF0C\u6D89\u53CA\u68C0\u7D22\u548C\u53EF\u80FD\u64CD\
  \u7EB5\u4ECA\u5929\u7684\u65E5\u671F\u548C\u65F6\u95F4\u3002\u7A0B\u5E8F\u5458\u6267\
  \u884C\u6B64\u64CD\u4F5C\u662F\u4E3A\u4E86\u5728\u7F51\u7AD9\u3001\u5E94\u7528\u7A0B\
  \u5E8F\u4E2D\u663E\u793A\u65E5\u671F\uFF0C\u8DDF\u8E2A\u7528\u6237\u4E92\u52A8\uFF0C\
  \u6216\u5904\u7406\u65F6\u95F4\u654F\u611F\u6570\u636E\u3002"
lastmod: '2024-03-13T22:44:48.226977-06:00'
model: gpt-4-0125-preview
summary: "\u5728JavaScript\u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u662F\u4E00\u9879\
  \u57FA\u672C\u4EFB\u52A1\uFF0C\u6D89\u53CA\u68C0\u7D22\u548C\u53EF\u80FD\u64CD\u7EB5\
  \u4ECA\u5929\u7684\u65E5\u671F\u548C\u65F6\u95F4\u3002\u7A0B\u5E8F\u5458\u6267\u884C\
  \u6B64\u64CD\u4F5C\u662F\u4E3A\u4E86\u5728\u7F51\u7AD9\u3001\u5E94\u7528\u7A0B\u5E8F\
  \u4E2D\u663E\u793A\u65E5\u671F\uFF0C\u8DDF\u8E2A\u7528\u6237\u4E92\u52A8\uFF0C\u6216\
  \u5904\u7406\u65F6\u95F4\u654F\u611F\u6570\u636E\u3002."
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
weight: 29
---

## 什么 & 为什么？
在JavaScript中获取当前日期是一项基本任务，涉及检索和可能操纵今天的日期和时间。程序员执行此操作是为了在网站、应用程序中显示日期，跟踪用户互动，或处理时间敏感数据。

## 如何实现：
在原生JavaScript中，使用`Date`对象来处理日期和时间。以下是获取当前日期和时间的方法：

```javascript
const currentDate = new Date();
console.log(currentDate); // 示例输出：Fri Apr 14 2023 12:34:56 GMT+0100 (英国夏令时)
```

如果仅想以更友好的格式显示日期，你可以使用像`toLocaleDateString()`这样的方法：

```javascript
console.log(currentDate.toLocaleDateString()); // 示例输出：4/14/2023
```

如果想更精确地控制格式，那么像 *Moment.js* 或 *date-fns* 这样的第三方库非常受欢迎，尽管需要知道Moment.js现在被视为处于维护模式的遗留项目。

使用 *Moment.js*：

```javascript
const moment = require('moment'); // 假设使用Node.js或使用模块打包器
const formattedDate = moment().format('YYYY-MM-DD');
console.log(formattedDate); // 示例输出：2023-04-14
```

使用 *date-fns*，它强调模块化，允许您仅导入所需内容：

```javascript
const { format } = require('date-fns');
const formattedDate = format(new Date(), 'yyyy-MM-dd');
console.log(formattedDate); // 示例输出：2023-04-14
```

每种方法都提供了不同级别的方便性和灵活性，用于在JavaScript中处理日期，从内置的`Date`对象到通过库提供的更复杂的格式化和操作能力。

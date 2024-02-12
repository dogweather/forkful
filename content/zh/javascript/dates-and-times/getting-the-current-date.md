---
title:                "获取当前日期"
aliases:
- /zh/javascript/getting-the-current-date.md
date:                  2024-02-03T19:09:47.787765-07:00
model:                 gpt-4-0125-preview
simple_title:         "获取当前日期"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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

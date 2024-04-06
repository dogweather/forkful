---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:47.787765-07:00
description: "\u5982\u4F55\u5B9E\u73B0\uFF1A \u5728\u539F\u751FJavaScript\u4E2D\uFF0C\
  \u4F7F\u7528`Date`\u5BF9\u8C61\u6765\u5904\u7406\u65E5\u671F\u548C\u65F6\u95F4\u3002\
  \u4EE5\u4E0B\u662F\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u548C\u65F6\u95F4\u7684\u65B9\
  \u6CD5\uFF1A."
lastmod: '2024-04-05T21:53:48.503573-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
weight: 29
---

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

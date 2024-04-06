---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:07.230266-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A JavaScript\u539F\u751F\u63D0\u4F9B\u4E86\
  `Date.parse()`\u65B9\u6CD5\u548C`Date`\u6784\u9020\u51FD\u6570\u6765\u89E3\u6790\
  \u65E5\u671F\u5B57\u7B26\u4E32\u3002\u7136\u800C\uFF0C\u8FD9\u4E9B\u65B9\u6CD5\u5728\
  \u4E0D\u540C\u6D4F\u89C8\u5668\u4E2D\u5B58\u5728\u5C40\u9650\u6027\u548C\u4E0D\u4E00\
  \u81F4\u6027\uFF0C\u7279\u522B\u662F\u5BF9\u4E8E\u975E\u6807\u51C6\u65E5\u671F\u683C\
  \u5F0F\u3002\u4E3A\u4E86\u89E3\u51B3\u8FD9\u4E9B\u95EE\u9898\uFF0C\u7B2C\u4E09\u65B9\
  \u5E93\u5982`Moment.js`\u548C`date-fns`\u56E0\u5176\u5F3A\u5927\u7684\u529F\u80FD\
  \u548C\u6613\u7528\u6027\u800C\u53D7\u5230\u6B22\u8FCE\u3002"
lastmod: '2024-03-13T22:44:48.220375-06:00'
model: gpt-4-0125-preview
summary: "JavaScript\u539F\u751F\u63D0\u4F9B\u4E86`Date.parse()`\u65B9\u6CD5\u548C\
  `Date`\u6784\u9020\u51FD\u6570\u6765\u89E3\u6790\u65E5\u671F\u5B57\u7B26\u4E32\u3002\
  \u7136\u800C\uFF0C\u8FD9\u4E9B\u65B9\u6CD5\u5728\u4E0D\u540C\u6D4F\u89C8\u5668\u4E2D\
  \u5B58\u5728\u5C40\u9650\u6027\u548C\u4E0D\u4E00\u81F4\u6027\uFF0C\u7279\u522B\u662F\
  \u5BF9\u4E8E\u975E\u6807\u51C6\u65E5\u671F\u683C\u5F0F\u3002\u4E3A\u4E86\u89E3\u51B3\
  \u8FD9\u4E9B\u95EE\u9898\uFF0C\u7B2C\u4E09\u65B9\u5E93\u5982`Moment.js`\u548C`date-fns`\u56E0\
  \u5176\u5F3A\u5927\u7684\u529F\u80FD\u548C\u6613\u7528\u6027\u800C\u53D7\u5230\u6B22\
  \u8FCE."
title: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F"
weight: 30
---

## 如何操作：
JavaScript原生提供了`Date.parse()`方法和`Date`构造函数来解析日期字符串。然而，这些方法在不同浏览器中存在局限性和不一致性，特别是对于非标准日期格式。为了解决这些问题，第三方库如`Moment.js`和`date-fns`因其强大的功能和易用性而受到欢迎。

### 使用原生JavaScript:
```javascript
const dateString = "2023-04-30T14:55:00";
const dateObj = new Date(dateString);

console.log(dateObj);  // 输出：Sun Apr 30 2023 14:55:00 GMT+0000（协调世界时）
```

### 使用Moment.js:
首先，通过npm安装Moment.js或将其包含在您的项目中。然后：
```javascript
const moment = require('moment');

const dateString = "2023-04-30T14:55:00";
const dateObj = moment(dateString);

console.log(dateObj.toString());  // 输出：Sun Apr 30 2023 14:55:00 GMT+0000
```

### 使用date-fns：
在将`date-fns`添加到您的项目后，像这样解析日期字符串：
```javascript
const { parseISO } = require('date-fns');

const dateString = "2023-04-30T14:55:00";
const dateObj = parseISO(dateString);

console.log(dateObj);  // 输出：2023-04-30T14:55:00.000Z
```

`Moment.js`和`date-fns`都提供了更全面的解析能力，包括处理各种格式和地区设置，这使得它们更适合复杂的应用程序。

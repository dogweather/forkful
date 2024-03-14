---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:07.230266-07:00
description: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F\u5141\u8BB8\u7A0B\u5E8F\
  \u5458\u5C06\u6587\u672C\u65E5\u671F\u8868\u793A\u8F6C\u6362\u4E3AJavaScript `Date`\u5BF9\
  \u8C61\uFF0C\u4EE5\u4FBF\u4E8E\u8FDB\u884C\u65E5\u671F\u64CD\u4F5C\u3001\u6BD4\u8F83\
  \u548C\u683C\u5F0F\u5316\u64CD\u4F5C\u3002\u8FD9\u4E2A\u8FC7\u7A0B\u5BF9\u4E8E\u5904\
  \u7406\u7528\u6237\u8F93\u5165\u3001\u5904\u7406\u6570\u636E\u5E93\u4E2D\u7684\u6570\
  \u636E\u6216\u4E0E\u4EE5\u5B57\u7B26\u4E32\u683C\u5F0F\u901A\u4FE1\u65E5\u671F\u7684\
  APIs\u5DE5\u4F5C\u81F3\u5173\u91CD\u8981\u3002"
lastmod: '2024-03-13T22:44:48.220375-06:00'
model: gpt-4-0125-preview
summary: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F\u5141\u8BB8\u7A0B\u5E8F\
  \u5458\u5C06\u6587\u672C\u65E5\u671F\u8868\u793A\u8F6C\u6362\u4E3AJavaScript `Date`\u5BF9\
  \u8C61\uFF0C\u4EE5\u4FBF\u4E8E\u8FDB\u884C\u65E5\u671F\u64CD\u4F5C\u3001\u6BD4\u8F83\
  \u548C\u683C\u5F0F\u5316\u64CD\u4F5C\u3002\u8FD9\u4E2A\u8FC7\u7A0B\u5BF9\u4E8E\u5904\
  \u7406\u7528\u6237\u8F93\u5165\u3001\u5904\u7406\u6570\u636E\u5E93\u4E2D\u7684\u6570\
  \u636E\u6216\u4E0E\u4EE5\u5B57\u7B26\u4E32\u683C\u5F0F\u901A\u4FE1\u65E5\u671F\u7684\
  APIs\u5DE5\u4F5C\u81F3\u5173\u91CD\u8981\u3002"
title: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F"
---

{{< edit_this_page >}}

## 什么 & 为什么？
从字符串解析日期允许程序员将文本日期表示转换为JavaScript `Date`对象，以便于进行日期操作、比较和格式化操作。这个过程对于处理用户输入、处理数据库中的数据或与以字符串格式通信日期的APIs工作至关重要。

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

---
title:                "获取当前日期"
date:                  2024-01-20T15:17:10.357283-07:00
html_title:           "Bash: 获取当前日期"
simple_title:         "获取当前日期"

category:             "TypeScript"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (什么以及为什么?)
获取当前日期是读取系统时间来知道现在的年月日。程序员这么做来记录事件发生的时间、验证数据或者为用户展示。

## How to: (如何操作:)
```TypeScript
// 获取当前日期示例
const now = new Date();
console.log(now.toDateString()); // 输出类似 "Wed Mar 24 2021"

// 更详细的时间
console.log(now.toLocaleString()); // 输出类似 "3/24/2021, 1:00:00 PM"
```

样例输出：

```
Wed Mar 24 2021
3/24/2021, 1:00:00 PM
```

## Deep Dive (深入探讨)
JavaScript和TypeScript中获取当前日期是用`Date`对象。1995年引入JavaScript, `Date`就已存在。对比其他语言，如Python的`datetime`模块，`Date`更简单但功能受限。

替代方案包括使用库如Moment.js，它提供更多格式化和处理日期的功能。但因体积大，部分开发者转向Day.js等轻量级库。

`Date`对象直接连接浏览器或Node.js的系统时钟，使用时要注意时区和夏令时的影响。

## See Also (另见)
- [MDN Web Docs: Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)
- [Day.js](https://day.js.org/)

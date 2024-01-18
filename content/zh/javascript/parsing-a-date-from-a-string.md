---
title:                "从字符串中解析日期"
html_title:           "Javascript: 从字符串中解析日期"
simple_title:         "从字符串中解析日期"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
日期字符串分析是指将字符串中包含的日期信息转换为可读的日期格式。程序员这么做的原因是为了能够更容易地处理日期数据，并将其用于各种计算和比较操作。

## 怎样操作：
下面是一个简单的例子，展示了如何使用 `Date.parse()` 方法将日期字符串转换为日期对象，并输出转换后的结果。 

```Javascript
let dateString = "2020-06-15";
let dateObject = new Date(Date.parse(dateString));
console.log(dateObject);
// 输出结果：Mon Jun 15 2020 00:00:00 GMT+0800 (China Standard Time)
```

## 深入了解：
日期字符串分析在计算机编程领域已经存在了很长一段时间。在早期，程序员们需要手动处理日期数据，这往往导致了各种错误和混乱。现在，使用 `Date.parse()` 方法可以方便地将字符串中的日期信息提取出来，并转换为标准的日期对象。在某些特殊情况下，也可以使用其他方法来解析日期字符串，例如正则表达式。

## 参考链接：
- [MDN - Date.parse() 方法](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Date/parse)
- [W3School - Date.parse() 方法](https://www.w3school.com.cn/jsref/jsref_parse.asp)
- [阮一峰 - JavaScript日期对象简介](https://www.ruanyifeng.com/blog/2011/11/javascript_date_time.html)
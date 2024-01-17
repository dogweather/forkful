---
title:                "将日期转换为字符串"
html_title:           "TypeScript: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
将一个日期转换为字符串是指将一个日期值转换为可读的文本格式。程序员通常这么做是为了方便阅读和比较不同的日期。

## 如何：
```TypeScript
const date = new Date();
console.log(date.toString());
// Output: Wed Mar 10 2021 12:00:00 GMT+0800 (中国标准时间)
```

```TypeScript
const options = { year: 'numeric', month: 'numeric', day: 'numeric' };
const date = new Date();
console.log(date.toLocaleDateString('zh-CN', options));
// Output: 2021/3/10
```

## 深入探讨：
转换日期为字符串的历史可以追溯到计算机编程的早期。在早期计算机系统中，日期和时间通常以数字表示，不便于理解和比较。随着计算机技术的发展，日期转换为字符串可以使用不同的格式来满足不同的需求。另外，除了使用内置的 `toString()` 和 `toLocaleDateString()` 方法，也可以使用第三方库来完成这个功能。

## 参考链接：
- [TypeScript Documents](https://www.typescriptlang.org/docs)
- [MDN Web Docs - Date](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Date)
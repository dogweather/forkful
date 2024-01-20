---
title:                "将日期转换为字符串"
html_title:           "Bash: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 是什么 & 为什么？
日期转字符串是将日期的表示方式从日期对象变为文本格式的过程。程序员经常需要这么做，因为字符串便于显示和存储，对于用户接口和数据库操作有很大的便利性。

## 如何操作：
在 TypeScript 中，使用 `toISOString()` 方法可以很容易地将日期转换为字符串。定义一个日期，然后使用 `toISOString()` 就可以了，代码如下：

```TypeScript
let currentDate: Date = new Date();
let stringDate: string = currentDate.toISOString();

console.log(stringDate);
```

你会在控制台看到如下格式的输出：
```Output
"2021-05-10T14:48:00.000Z"
```

## 深入了解
早在计算机发展初期，有关日期的处理就存在。原始的 Unix 时间戳就是以字符串的形式存储的，例如 "1577865600"。使用字符串可以方便在不同的程序和系统间共享数据。

Javascript 的 Date 对象提供了多种将日期转换为字符串的方法。除了 `toISOString()` 之外，还有 `toDateString()`, `toJSON()` 等。你可以根据需要选择不同的方法。

使用 `toISOString()` 方法会将日期转换为 ISO 8601 格式的字符串，这是一个国际标准。这是最常用的日期字符串格式，也是大部分编程语言和系统默认的日期字符串格式。

## 另请查看
- 详细了解 JavaScript 的 Date 对象：https://www.w3schools.com/js/js_date_methods.asp
- ISO 8601 标准：https://www.iso.org/iso-8601-date-and-time-format.html
- Unix 时间戳：https://en.wikipedia.org/wiki/Unix_time
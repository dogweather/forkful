---
title:                "获取当前日期"
html_title:           "Javascript: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 什么是当前日期？为什么程序员要获取它？

当前日期指的是当天的日期，程序员通常会使用相关函数来获取它。这样做的原因是因为在软件开发中，需要记录或处理日期和时间相关的数据，以便跟踪和记录事件的发生时间等。

## 如何获取当前日期：

```Javascript
const today = new Date();
console.log(today);
```

这里我们使用了Date对象的构造函数来创建一个新的对象并将它赋值给变量today。然后使用console.log()函数来打印输出结果，即当前的日期。

输出结果可能类似于：Sun Mar 28 2021 15:02:07 GMT+0800 (China Standard Time)

## 深入了解：

1. 历史背景：在早期的计算机系统中，日期和时间通常是以固定格式存储的，因此编程人员需要编写复杂的代码来处理日期和时间数据。随着现代计算机系统的发展，内置的日期和时间函数使得获取当前日期变得更加简单和方便。

2. 替代方法：除了使用Date对象，也可以使用其他第三方库来获取当前日期。例如，Moment.js是一个流行的专门用来处理日期和时间的库，它提供了更多的功能和选项。

3. 实现细节：Date对象的构造函数可以接受参数来定义日期的格式。而输出的结果也可以通过调用不同的方法来改变日期的显示方式，如getDate()来获取具体的天数，getMonth()来获取月份等等。

## 参考链接：

- [JavaScript Date对象文档](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js官方网站](https://momentjs.com/)
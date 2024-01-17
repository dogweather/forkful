---
title:                "比较两个日期"
html_title:           "TypeScript: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 什么和为什么？
对比两个日期是指将两个日期进行比较，通常是指比较它们的年、月、日或具体时间。程序员这样做是为了在处理日期数据时能够得到精确的结果。

# 怎么做：
使用TypeScript编写代码：

```
// 创建两个日期对象
let date1 = new Date('2021-01-12');
let date2 = new Date('2021-01-09');

// 比较两个日期，如果date1晚于date2，则返回1，如果相等则返回0，如果date1早于date2，则返回-1
let result: number = date1.getTime() - date2.getTime();
if(result > 0) {
    console.log("日期1晚于日期2");
} else if (result < 0) {
    console.log("日期1早于日期2");
} else {
    console.log("两个日期相等");
}
```

输出:

```
日期1早于日期2
```

# 深入了解：
在编程中，比较日期是为了提高程序的准确性和效率。在历史上，人们使用的日历系统不同，导致日期间的比较非常困难。而现在，我们可以使用计算机语言来比较日期，帮助我们快速地处理日期数据。除了比较两个日期对象之外，我们还可以使用时间戳或日期字符串来比较日期。

# 参考资料：
- [日期函数 - TypeScript](https://www.typescriptlang.org/docs/handbook/dates-and-times.html)
- [原生JavaScript日期比较的指令](https://www.javatpoint.com/javascript-date-comparison)
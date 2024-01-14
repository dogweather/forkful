---
title:    "Javascript: 将日期转换为字符串"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

# 为什么要将日期转换为字符串（Why）

将日期转换为字符串是一项常见的任务，特别是在web开发和数据处理中。通过将日期转换为字符串，我们可以更方便地在不同平台和应用程序之间共享或展示日期信息。例如，在网页上显示发布日期，或在数据库中存储日期时，通常都需要将日期转换为字符串的形式。

# 如何将日期转换为字符串（How To）

在Javascript中，我们可以使用内置方法`toString()`来将日期对象转换为字符串。例如：

```
// 创建一个日期对象
let date = new Date("2021-10-23");

// 使用toString()方法将日期转换为字符串
let dateString = date.toString();
console.log(dateString); // 输出：'Sat Oct 23 2021 00:00:00 GMT+0800 (中国标准时间)'
```

我们也可以使用更灵活的`toLocaleString()`方法来转换日期为本地化的字符串。该方法接受两个参数，第一个参数为本地化选项（如'zh-CN'表示简体中文），第二个参数为选项对象，用于自定义日期格式。例如：

```
// 创建一个日期对象
let date = new Date("2021-10-23");

// 使用toLocaleString()方法将日期转换为本地化的字符串
let dateString = date.toLocaleString('zh-CN', { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' });
console.log(dateString); // 输出：'星期六，2021年10月23日'
```

# 深入理解日期转换为字符串（Deep Dive）

在Javascript中，日期对象内部存储为以毫秒表示的时间戳（自1970年1月1日00:00:00 UTC至今的毫秒数），而不是以特定格式存储。因此，当我们调用`toString()`或`toLocaleString()`方法时，实际上是通过内部`Date.prototype.toString()`和`Date.prototype.toLocaleString()`方法来生成日期对象的字符串表示。

在这些内部方法中，我们可以使用一系列的占位符来指定日期的格式，例如`'yyyy'`表示四位数的年份，`'MM'`表示两位数的月份。此外，还可以使用`'EEE'`表示星期的简写，`'EEEE'`表示星期的全称。完整的占位符列表可以在 [MDN文档](https://developer.mozilla.org/zh-CN/docs/orphaned/Web/JavaScript/Reference/Global_Objects/Date/toLocaleString) 中找到。

参考答案中的例子，我们使用`{ weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' }`来自定义日期的格式，也可以根据需要自由组合占位符来生成不同形式的字符串。

# 参考链接（See Also）

- [MDN文档：Date.prototype.toString()](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Date/toString)
- [MDN文档：Date.prototype.toLocaleString()](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleString)
- [MDN文档：日期占位符列表](https://developer.mozilla.org/zh-CN/docs/orphaned/Web/JavaScript/Reference/Global_Objects/Date/toLocaleString)
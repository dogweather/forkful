---
title:    "Gleam: 获取当前日期"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

为什么：获取当前日期的原因

在编程中，获取当前日期是一项常见的任务。无论是在开发网站还是应用程序，都有可能需要使用当前日期来进行一些操作。比如，网站的登录页面可能需要显示最后一次登陆的日期，或者应用程序可能需要记录每天的数据统计。因此，获取当前日期在编程中是非常有用的。

如何做到：示例代码和输出

```Gleam
// 导入日期库
import gleam / date / Date

// 获取当前日期
let today = Date.current()

// 输出当前日期
IO.print("今天的日期是: ${Date.to_string(today)}")
```

```Gleam
// 导入日期库
import gleam / date / Date

// 获取当前日期和时间
let now = Date.time()

// 输出当前时间
IO.print("当前时间是: ${Date.to_string(now)}")
```

深入介绍：关于获取当前日期的更多信息

除了上面提到的简单示例，我们还可以通过日期库来获取更多关于当前日期的信息。例如，我们可以通过日期库中的函数来判断某一天是否是闰年，或者计算两个日期之间的天数差。这些功能都可以帮助我们更方便地使用当前日期，并且让我们的程序更加灵活。

参考资料

- Gleam 日期库文档：https://gleam.run/libraries/date/
- 日期与时间在编程中的应用：https://zhuanlan.zhihu.com/p/55709226
- 如何使用当前日期来生成文件名：https://www.cnblogs.com/yjf512/p/6462523.html

参见

#### 参考链接
---
title:    "Ruby: 计算未来或过去的日期"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 为什么
为什么我们要计算未来或过去的日期呢？这可能是因为我们需要在一个特定的日期做出决定，或者我们需要知道某个事件发生的具体日期。不管原因如何，计算日期的功能对于编程来说是非常有用的。

## 怎么做
要在Ruby中计算未来或过去的日期，我们需要使用Date类和它的相关方法。下面是一个简单的代码示例，我们将以当前日期为基础，计算一周后的日期，并将结果打印出来：

```Ruby
require 'date'
today = Date.today
future_date = today + 7
puts future_date
```

运行上面的程序，你将得到类似这样的输出：2019-02-18。我们也可以使用上面的代码来计算过去的日期，只需要改变“+”符号为“-”符号即可。

## 深入探讨
在计算日期时，我们需要考虑闰年、月份的天数等因素。例如，如果我们想要计算未来某个月的最后一天，我们可以使用下面的代码：

```Ruby
require 'date'
today = Date.today
future_date = today + 1.month
last_day = future_date.end_of_month
puts last_day
```

这里的end_of_month方法会自动计算指定月份的最后一天，无论这个月份有多少天。

## 参考资料
- [Ruby日期计算教程](https://www.rubyguides.com/2015/03/ruby-date-time-tutorial/)
- [Date类文档](https://ruby-doc.org/stdlib-2.6.1/libdoc/date/rdoc/Date.html)
- [Ruby on Rails中的日期计算](https://guides.rubyonrails.org/active_support_core_extensions.html#time-and-date-calculations)

## 参见
- [Markdown教程](https://www.markdownguide.org/basic-syntax/)
- [Markdown语法指南](https://ruby-china.org/wiki/markdown-syntax)（中文版）
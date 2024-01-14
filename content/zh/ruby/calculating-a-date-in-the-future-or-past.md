---
title:                "Ruby: 未来或过去计算日期"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 为什么

计算未来或过去日期似乎是一个古老的编程问题，但它仍然是一个实用的技巧。无论是在编写日历应用程序还是在处理付款截止日期，计算日期是非常常见的需求。使用Ruby编程语言，我们可以轻松地解决这个问题。

## 怎样做

要计算未来或过去的日期，我们需要使用Ruby的`Date`类。首先，我们需要创建一个日期对象，指定日期的年份、月份和日。例如，如果我们想要计算5天后的日期，我们可以使用以下代码：

```Ruby
require 'date'

today = Date.today # 获取今天日期
future_date = today + 5 # 计算5天后的日期

puts future_date # 输出：2019-12-31
```

温馨提示：`Date.today`方法用于获取当前日期。您也可以使用`Date.new(年份, 月份, 日)`方法手动创建日期对象。

如果我们想要计算过去的日期，同样可以使用`-`运算符，例如：

```Ruby
require 'date'

today = Date.today # 获取今天日期
past_date = today - 10 # 计算10天前的日期

puts past_date # 输出：2019-12-16
```

我们也可以使用其他方法来计算日期，例如`next_day`、`prev_day`、`next_month`等等。更多关于`Date`类的方法可以查阅官方文档。

## 深入探讨

使用Ruby计算日期还有更多有趣的用途，例如计算两个日期之间的天数：

```Ruby
require 'date'

future_date = Date.new(2020, 1, 10) # 创建一个未来日期对象
today = Date.today # 获取今天日期

days_remaining = (future_date - today).to_i # 计算剩余天数

puts days_remaining # 输出：11
```

您也可以使用`Date`类的`parse`方法来将日期字符串转换为日期对象，例如：

```Ruby
require 'date'

future_date = Date.parse("2020-01-10") # 将日期字符串转换为日期对象
```

## 参考资料

- 官方文档：https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html
- 计算日期间隔：https://knavitkumar.blogspot.com/2018/06/ruby-date-manipulation-calculating-date.html
- 使用`differnce`方法计算日期间隔：https://mathewsanders.com/sandbox/ormin-ruby-journey/ruby-tips-7-date-difference-in-days/
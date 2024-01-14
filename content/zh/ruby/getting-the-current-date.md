---
title:                "Ruby: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 为什么

在编程中，获取当前日期和时间是很常见的需求。当我们想要在程序中记录特定事件发生的时间，或者想要展示当前的日期和时间给用户时，我们都需要获取当前的日期。因此，学习如何在Ruby中获取当前日期是很有用的。

# 如何

在Ruby中，我们可以使用内置的`Time`类来获取当前日期和时间。这个类提供了很多方法来获取不同格式的日期和时间，比如`now`方法可以返回一个当前的`Time`对象。

```Ruby
# 获取当前日期和时间
current_time = Time.now
# 输出结果： 2021-10-15 15:30:45 +0800
puts current_time

# 获取当前日期和时间的不同格式
# 年-月-日
current_date = current_time.strftime("%Y-%m-%d")
puts current_date
# 输出结果：2021-10-15

# 月/日/年 小时:分钟AM/PM
current_datetime = current_time.strftime("%m/%d/%Y %I:%M%p")
puts current_datetime
# 输出结果：10/15/2021 03:30PM
```

要注意的是，`strftime`方法可以让我们自定义日期和时间的输出格式。具体的格式转换字符串可以在[Ruby官方文档](https://ruby-doc.org/core-3.0.2/Time.html#method-i-strftime)中找到。

# 深入了解

除了使用`Time`类，我们也可以使用`Date`类来获取只包含日期的当前日期对象。同样地，`Date`类也提供了很多方法来获取不同格式的日期。

```Ruby
# 获取当前日期
current_date = Date.today
# 输出结果： #<Date: 2021-10-15 ((2459517j,0s,0n),+0s,2299161j)>

# 获取当前日期的不同格式
# 年-月-日
current_date_format = current_date.strftime("%Y-%m-%d")
puts current_date_format
# 输出结果：2021-10-15

# 只获取年份
current_year = current_date.year
puts current_year
# 输出结果：2021

# 获取指定日期对应的星期几（1-7分别对应周一至周日）
day_of_week = current_date.cwday
puts day_of_week
# 输出结果：5 （表示今天是星期五）
```

# 参考

- [Ruby官方文档](https://ruby-doc.org/core-3.0.2/Time.html)
- [Ruby中的Date类](https://ruby-doc.org/stdlib-3.0.2/libdoc/date/rdoc/Date.html)
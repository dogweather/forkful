---
title:                "Ruby: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

##为什么

在Ruby编程中，有时候我们需要把日期转换成字符串。这可以用来方便地在程序中显示或处理日期数据。因此，学习如何将日期转换成字符串是非常有用的。

##如何做

为了将日期转换成字符串，我们可以使用Ruby的`.to_s`方法。此外，我们还可以使用`.strftime`方法来自定义日期格式。

```Ruby
# 使用to_s方法将日期转换成字符串
date = Time.new(2020, 9, 30)
puts date.to_s

# 输出：2020-09-30 00:00:00 +0800

# 使用strftime方法自定义日期格式
date = Time.new(2020, 9, 30)
puts date.strftime("%Y/%m/%d")

# 输出：2020/09/30
```

##深入探讨

要理解如何将日期转换成字符串，我们需要了解日期和时间在Ruby中是如何被表示的。Ruby中的日期和时间实际上是用一个数值来表示的，称为“时间戳”。这个数值代表自1970年1月1日午夜（格林威治标准时间）以来的秒数。因此，将日期转换成字符串实际上就是将这个秒数格式化成我们想要的样式。

##参考链接

- [Ruby中日期和时间的表示](https://ruby-doc.org/core-2.6/Time.html#method-c-new)
- [Ruby中.to_s方法的用法](https://ruby-doc.org/core-2.6/Time.html#method-i-to_s)
- [Ruby中.strftime方法的用法](https://ruby-doc.org/core-2.6/Time.html#method-i-strftime)

##看一下

如果您想了解更多有关Ruby中日期和时间的知识，可以参考以下链接：

- [Ruby标准库中的日期和时间](https://ruby-doc.org/stdlib-2.6/libdoc/date/rdoc/index.html)
- [如何在Ruby中处理日期和时间](https://www.digitalocean.com/community/tutorials/how-to-work-with-date-and-time-in-ruby)
- [Ruby中日期转换的实际应用](https://www.rubyguides.com/2019/11/date-and-time-in-ruby/)
---
title:    "Ruby: 计算未来或过去的日期"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## 为什么

有时候，我们需要计算未来或过去的日期，这可能是因为做年度计算或制作时间表。在Ruby中，我们可以轻松地使用内置的Date类来进行日期计算，让我们来看看如何做到这一点吧！

## 怎么做

首先，我们需要导入Date类，然后使用`.today`方法来获取当前日期。现在，让我们来看一个例子，假设我们需要计算未来7天的日期，可以这样做：

```
require 'date'

today = Date.today
future_date = today + 7

puts "The date in 7 days will be: #{future_date}"
```

输出将会是：

```
The date in 7 days will be: 2021-05-04
```

同理，我们也可以计算过去的日期，比如计算7天前的日期，只需要改变一下操作符即可：

```
require 'date'

today = Date.today
past_date = today - 7

puts "The date 7 days ago was: #{past_date}"
```

输出将会是：

```
The date 7 days ago was: 2021-04-20
```

## 深入了解

除了简单的加减操作之外，我们还可以使用`.next`和`.prev`方法来计算下一个或上一个日期。另外，Date类还提供了其他有用的方法，比如`.strftime`可以根据指定的格式将日期转换为字符串。如果想要了解更多关于Date类的信息，可以参考[Ruby官方文档](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html)。

## 参考链接

- [Ruby官方文档](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html)
- [Ruby Date类的使用方法](https://www.rubyguides.com/2019/10/ruby-date/)
- [如何在Ruby中计算未来和过去的日期](https://www.geeksforgeeks.org/how-to-calculate-future-and-past-date-using-ruby/)
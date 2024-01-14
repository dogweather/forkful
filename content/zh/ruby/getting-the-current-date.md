---
title:                "Ruby: 获取当前日期"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么

在编写程序时，经常需要使用当前的日期，例如记录日志或进行数据分析。获取当前日期也有助于跟踪程序的执行时间。因此，掌握如何获取当前日期在Ruby编程中是非常重要的。

## 如何做

在Ruby中，有几种方法可以获取当前日期。下面是几个示例，以及每个示例的输出结果。

```ruby
# 方法一：使用Time类的now方法
current_date = Time.now
puts current_date
# 输出：2021-03-22 11:35:17 +0800

# 方法二：使用Date类的today方法
current_date = Date.today
puts current_date
# 输出：#<Date: 2021-03-22 ((2459296j,0s,0n),+0s,2299161j)>

# 方法三：使用DateTime类的now方法
current_date = DateTime.now
puts current_date
# 输出：2021-03-22T11:36:50+08:00

# 方法四：使用Kernel模块的方法
current_date = Time.new
puts current_date
# 输出：2021-03-22 11:37:35 +0800

# 方法五：使用Date类和strftime方法来自定义日期格式
current_date = Date.today.strftime("%Y-%m-%d")
puts current_date
# 输出：2021-03-22
```

## 深入了解

每个方法都有自己的特点和用途，例如Time类的now方法可以获取当前的日期和时间，但它也可以用于跟踪程序的执行时间。另外，通过使用Date类和strftime方法，我们可以自定义日期的格式，使其符合特定的需求。

在Ruby编程中，日期和时间类型分别为Time和DateTime，他们都提供了多种方法来操作日期和时间。深入了解这些类型和方法可以帮助我们更好地处理日期和时间相关的任务。

## 参考链接

- [Ruby官方文档 - Time类](https://ruby-doc.org/core-2.5.3/Time.html)
- [Ruby官方文档 - Date类](https://ruby-doc.org/stdlib-2.5.3/libdoc/date/rdoc/Date.html)
- [Ruby官方文档 - DateTime类](https://ruby-doc.org/stdlib-2.5.3/libdoc/date/rdoc/DateTime.html)
- [Ruby官方文档 - Kernel模块](https://ruby-doc.org/core-2.5.3/Kernel.html)
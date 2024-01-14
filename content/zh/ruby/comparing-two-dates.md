---
title:    "Ruby: 比较两个日期"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## 为什么

比较两个日期在任何编程语言中都是一项常见的任务，包括 Ruby。通过比较日期，您可以检查日期之间的关系，比如哪个日期更早或更晚。这对于许多不同的编程问题来说都是必不可少的。在本文中，我们将学习如何使用 Ruby 来比较两个日期。

## 如何

首先，您需要使用 Ruby 的 `Date` 类来表示日期。然后，为了比较两个日期，您可以使用 `>`、`<`、`==` 等比较运算符来比较日期对象。让我们来看一个示例：

```ruby
first_date = Date.new(2021, 1, 1)
second_date = Date.new(2021, 5, 20)

puts first_date > second_date  # 输出为 false
puts first_date < second_date  # 输出为 true
puts first_date == second_date # 输出为 false
```

在上面的示例中，我们首先创建了两个日期对象，然后使用比较运算符比较它们。请注意，年份、月份和日期参数分别是 `Date.new` 函数的第一个、第二和第三个参数。这是创建日期对象所需的最少参数。

您还可以使用 `between?` 方法来检查一个日期是否在另外两个日期之间。比如：

```ruby
first_date = Date.new(2021, 1, 1)
second_date = Date.new(2021, 5, 20)
third_date = Date.new(2021, 3, 15)

puts third_date.between?(first_date, second_date) # 输出为 true
```

上面的代码将检查 `third_date` 是否在 `first_date` 和 `second_date` 之间，结果将输出为 `true`。

除了以上方法，您还可以使用 `strftime` 方法来格式化日期对象，使其变得更易读。比如：

```ruby
today = Date.today

puts today.strftime("%B %d, %Y") # 输出为 "October 01, 2021"
```

在上面的代码中，`%B` 代表月份的全称，`%d` 代表日期，`%Y` 代表年份。您可以根据自己的需要使用不同的格式。

## 深入探讨

在 Ruby 中，日期对象有一个非常有用的方法，即 `prev_day` 和 `next_day`。这些方法可以让您在日期对象上进行加减操作，使日期向前或向后移动。比如：

```ruby
today = Date.today

puts today.prev_day   # 输出为 "2021-09-30"
puts today.next_day   # 输出为 "2021-10-02"
```

此外，日期对象还有一个 `sunday?` 方法来检查一个日期是否是星期天，以及 `weekday?` 方法来检查一个日期是否是工作日。比如：

```ruby
today = Date.today

puts today.sunday?   # 输出为 false
puts today.weekday?  # 输出为 true
```

## 参考资料

- [Ruby Date 类](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)
- [Ruby API 文档：比较运算符](https://ruby-doc.org/core-2.7.1/Comparable.html)
- [Ruby API 文档：日期和时间格式化](https://ruby-doc.org/core-2.7.1/DateTime.html#method-i-strftime)
- [Ruby API 文档：日期对象方法](https://ruby-doc.org/core-2.7.1/Date.html#method-i-sunday-3F)

## 参考链接

- [《Ruby on Rails 教程》](https://railstutorial-china.org/book/)
- [《Practical Object-Oriented Design: An Agile Primer Using Ruby》](http://www.poodr.com/)
- [Ruby 中文文档](https://www.ruby-lang.org/zh_cn/documentation/)
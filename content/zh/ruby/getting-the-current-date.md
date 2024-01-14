---
title:    "Ruby: 获取当前日期"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 为什么使用 Ruby 编程获取当前日期

获取当前日期是编写任何程序时都可能需要的基本操作。无论是为了显示日期时间，还是为了计算时间差异，都需要准确地获取当前日期。Ruby 语言提供了简便的方法来获取当前日期，让你的编程更加高效和精确。

## 如何使用 Ruby 获取当前日期

要获取当前日期，我们可以使用 Ruby 内置的 `Time`类，通过 `now`方法来获取。下面是一个简单的示例代码：

```Ruby
time_now = Time.now
puts "当前日期是：" + time_now.strftime("%m-%d-%Y")
```

运行以上代码，你将得到类似于这样的输出：

```
当前日期是：05-22-2021
```

通过 `strftime`方法，我们可以指定日期的显示格式。上面的代码中，`%m-%d-%Y`表示月份-日期-年份的格式。

## 深入了解获取当前日期

除了 `Time`类，Ruby 还提供了`Date`类来处理日期相关的操作。与`Time`类不同的是，`Date`类只关注日期部分，不包含时间信息。如果你只需要获取日期，而不需要时间，那么使用`Date`类会更加合适。

此外，`DateTime`类也是一个有用的类，它将日期和时间结合在一起。如果你需要同时获取日期和时间，可以考虑使用`DateTime`类。

另外，如果你想要获取特定时区的当前日期，可以使用 `Time` 和 `DateTime` 的 `now`方法后面加上 `localtime`和 `now`参数，来指定时区。例如，`Time.now.localtime("+08:00")`将返回当前日期的东八区时间。

## 参考链接

- [Ruby Time类文档](https://ruby-doc.org/core-3.0.1/Time.html)
- [Ruby Date类文档](https://ruby-doc.org/stdlib-3.0.1/libdoc/date/rdoc/Date.html)
- [Ruby DateTime类文档](https://ruby-doc.org/stdlib-3.0.1/libdoc/date/rdoc/DateTime.html)

# 参考链接
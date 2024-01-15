---
title:                "获取当前日期"
html_title:           "Ruby: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么会想要获取当前日期？

获取当前日期是非常常见的需求，无论是在编程中还是日常生活中。它可以帮助我们记录事件发生的时间，或者作为程序中的条件控制，使程序更加智能化。在 Ruby 中，获取当前日期也是很容易的。让我们来看看如何做到这一点。

## 如何操作

Ruby 提供了一个名为 `Time` 的类来处理时间和日期。要获取当前日期，可以使用 `Time.now` 方法。让我们在 `irb` 中试一试：

```Ruby
Time.now
```

这会返回一个包含当前日期和时间的 `Time` 对象。如果你只想要当前日期，可以使用 `strftime` 方法来格式化日期的输出。例如，要只获取当前日期的年月日，可以使用 `"%Y-%m-%d"` 格式。让我们来试一试：

```Ruby
Time.now.strftime("%Y-%m-%d")
```

这会返回一个字符串，如 "2021-08-01"。你也可以使用其他格式来获取不同的日期信息，如小时、分钟、秒等。更多的日期格式可以在 [Ruby文档](https://ruby-doc.org/core-2.7.1/Time.html#method-i-strftime) 中找到。

## 深入了解

在 Ruby 中，日期和时间是以格式化的字符串的形式存储的。当我们使用 `Time.now` 方法时，实际上是在获取一个包含当前日期和时间信息的字符串。当我们使用 `strftime` 方法时，我们可以根据自己的需要从字符串中提取特定的日期信息。此外，Ruby 还提供了许多其他有用的方法来处理日期和时间，如 `Date`、`DateTime`、`zone` 等。你可以查阅 [Ruby文档](https://ruby-doc.org/core-2.7.1/Time.html) 来了解更多关于日期和时间操作的信息。

## 参考

- [Ruby文档 - Time类](https://ruby-doc.org/core-2.7.1/Time.html)
- [Ruby文档 - Date类](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)
- [Ruby文档 - DateTime类](https://ruby-doc.org/stdlib-2.7.1/libdoc/datetime/rdoc/DateTime.html)
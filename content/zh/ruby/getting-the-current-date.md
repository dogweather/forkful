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

[开始markdown]

如果你是一位入门的 Ruby 程序员，你可能会遇到这个问题：如何获取当前的日期？在本文中，我们将探讨这个问题的答案，并告诉你为什么程序员需要做这件事情。

## 为什么 & 如何？

获取当前日期是一个常见的编程任务。程序员经常需要获取当前日期来进行日志记录、时间戳、计时等操作。通过获取当前日期，程序员可以确保他们的程序在不同的时间运行时具有准确的时间信息。

## 如何：

```ruby
require 'date'

# 获取当前日期
current_date = Date.today

# 获取当前日期的时间戳
current_timestamp = DateTime.now.to_time.to_i

# 获取当前日期和时间
current_datetime = DateTime.now
```

运行以上代码，你将得到类似于以下输出：

```text
# 获取当前日期
2020-10-17

# 获取当前日期的时间戳
1602886085

# 获取当前日期和时间
2020-10-17T12:48:17+00:00
```

## 深入了解：

Ruby 中，你可以使用内置的 `Date` 和 `DateTime` 类来获取当前日期。如果你想要更多更精细的日期操作，你可以使用 `Time` 和 `DateTime` 类。

另外，你也可以使用第三方的日期库，例如 ActiveSupport、Chronic 等来获取当前日期。它们提供了更多功能强大和便捷的日期操作方法。

关于获取当前日期的实现细节方面，它可能与操作系统和硬件架构相关。例如，在不同操作系统下，时间戳可能有不同的格式，因此你需要根据你的环境调整你的代码。

## 参考资料：

- Ruby 官方文档：https://ruby-doc.org
- ActiveSupport GitHub：https://github.com/rails/rails/tree/master/activesupport
- Chronic GitHub：https://github.com/mojombo/chronic
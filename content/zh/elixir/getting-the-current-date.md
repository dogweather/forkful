---
title:                "获取当前日期"
date:                  2024-01-20T15:13:46.283242-07:00
html_title:           "Bash: 获取当前日期"
simple_title:         "获取当前日期"

category:             "Elixir"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (什么以及为什么？)
获取当前日期是指让程序显示系统当前的日期。程序员这么做通常是要记录事件、处理日志或者日期相关的逻辑。

## How to: (如何操作：)
在Elixir中，获取当前日期可以使用`Date`模块。看下面例子：

```elixir
# 引入Date模块
import Date

# 获取当前日期
today = utc_today()

# 打印当前日期
IO.inspect(today)
```

这个代码会输出类似：

```
~D[2023-04-05]
```

## Deep Dive (深入探讨)
历史上，Elixir是由José Valim发起的，他是个Ruby社区的成员，目的是创建一个更高效、可伸缩的语言。在Elixir中获取当前日期与许多其他语言获取日期的方式类似。`utc_today/0`是获取世界标准时间(UTC)当前日期的一个方法，如果你需要时区支持，可以使用`Timex`这样的第三方库。实现的细节方面，Elixir的`Date`模块是构建在Erlang的`:calendar`模块上的，提供了简单易用的函数。

## See Also (另请参阅)
- Elixir官方文档：[Date](https://hexdocs.pm/elixir/Date.html)
- Timex库：[GitHub - bitwalker/timex](https://github.com/bitwalker/timex)

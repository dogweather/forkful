---
title:    "Elixir: 将日期转换为字符串"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么

在Elixir编程中，经常需要将日期转换为字符串格式。这样做的原因可能是为了方便存储、显示或者与其他系统进行交互。无论是哪种情况，将日期转换为字符串是非常有用的技能。

## 如何进行

要将日期转换为字符串，我们可以使用 `DateTime` 模块中的 `to_string` 函数。让我们来看一个简单的例子：

```elixir
DateTime.to_string(~N[2021-10-10 18:00:00])
```

运行这段代码，我们会得到如下输出：

```elixir
"2021-10-10 18:00:00"
```

但是，如果我们想要自定义日期的格式，该怎么办呢？这时可以使用 `DateTime` 模块中的 `format` 函数。比如，我们想要将日期转换为类似"10月10日，2021年"的格式，可以这样做：

```elixir
DateTime.format(~N[2021-10-10], "M月d日，yyyy年")
```

运行后，输出如下：

```elixir
"10月10日，2021年"
```

除了使用固定的日期，我们也可以使用当前的日期与时间。比如，我们想要将当前的日期转换为字符串，可以使用 `DateTime.utc_now` 函数：

```elixir
DateTime.utc_now() |> DateTime.to_string()
```

输出为当前的日期与时间的字符串。

## 深入了解

在Elixir中，日期与时间的表示采用了 `DateTime` 模块和 `NaiveDateTime` 模块。前者包含时区信息，后者不包含。在转换日期为字符串时，我们需要注意使用哪个模块，以免出现时区差异带来的错误。

此外，`to_string` 函数接受一个 `format` 参数，这个参数决定了最终输出的格式。通过查阅文档，我们可以知道有哪些格式化选项可用，并根据自己的需要进行转换。

## 参考链接

- [Elixir官方文档 - DateTime模块](https://hexdocs.pm/elixir/DateTime.html)
- [Elixir官方文档 - DateTime模块](https://hexdocs.pm/elixir/NaiveDateTime.html)
- [Elixir官方文档 - DateTime模块](https://hexdocs.pm/elixir/DateTime.Format.html)
- [Elixir官方文档 - DateTime模块](https://hexdocs.pm/elixir/DateTime.to_string.html)
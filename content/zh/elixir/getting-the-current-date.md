---
title:                "获取当前日期."
html_title:           "Elixir: 获取当前日期."
simple_title:         "获取当前日期."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

什么是获取当前日期，为什么程序员需要做这件事？

获取当前日期是指获取当前日历日期及时间的功能。在程序中，我们经常需要使用当前日期来记录、跟踪或者比较不同的时间点，以便于实现各种功能。因此，程序员经常需要获取当前日期来确保程序的正常运行。

如何操作？

想要获取当前日期，我们可以使用 Elixir 标准库中的 `DateTime` 模块，它提供了丰富的日期和时间处理功能。以下是一个简单的示例代码：

```Elixir
Elixir 生成了 DateTime 对象，显示当前日期和时间
iex> DateTime.utc_now() 
{:ok, #DateTime<2019-08-07 07:37:43Z>}

Elixir 生成了当前日期
iex> DateTime.to_date(DateTime.utc_now()) 
{:ok, #Date<2019-08-07>}
```

在上述代码中，我们首先调用了 `DateTime.utc_now()` 方法来获得当前的日期和时间，然后通过 `DateTime.to_date()` 方法将其转换为日期对象，并最终输出当前的日期。可以看到，Elixir 提供了简单且方便的方法来获取当前日期。

深入探讨

获取当前日期在软件开发中非常重要，因为它可以帮助我们记录重要的时间信息，并且在需要的时候对时间进行比较和处理。在 Elixir 之前的其他语言中，常用的获取当前日期的方法包括使用系统命令或者调用操作系统的 API 来获得当前时间信息。而在 Elixir 中，我们可以直接使用 `DateTime` 模块来完成这一操作，使得获取当前日期更加简单和高效。

另外，除了 `DateTime` 模块，还有其他一些第三方库也提供了获取当前日期的功能，如 `Calendar`、 `Timex` 等，开发者可以选择根据自己的需求来使用不同的库。

查看更多

如果你想了解有关 Elixir 中日期和时间处理的更多信息，建议查看官方文档，其中包含了详细的说明和使用示例。同时，如果你对其他语言中的日期和时间处理方式感兴趣，也可以查看相关文档来进一步学习。

在这篇文章中，我们简单介绍了获取当前日期的意义和方法，并探讨了其在软件开发中的重要性。希望这可以帮助您更好地理解和使用日期和时间处理功能，使您的程序更加强大和可靠。
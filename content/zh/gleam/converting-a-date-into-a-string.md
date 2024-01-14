---
title:                "Gleam: 将日期转换为字符串"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么

在编程中，我们经常需要将日期转换为字符串来满足不同的需求。这可能是为了更好的显示，或是为了方便存储和处理数据。无论何种原因，将日期转换为字符串是一个常见的操作。

## 如何进行

Gleam提供了方便的方法来将日期转换为字符串。下面是一个示例代码，展示了如何使用`date`函数来获取当前日期，然后使用`string.from_date`函数将其转换为字符串。

```Gleam
let today = date()
let formatted_date = string.from_date(today)
```

这里，我们首先使用`date`函数来获取当前日期，并将其赋值给变量`today`。然后，我们使用`string.from_date`函数来将`today`转换为字符串，并将结果赋值给`formatted_date`变量。最后，我们可以使用`formatted_date`来显示或存储转换后的日期字符串。

以上代码的输出结果将会是类似于`"2020-09-03T00:00:00Z"`格式的字符串，它包含了日期和时间信息。如果我们想要自定义输出的字符串格式，可以在`string.from_date`函数中使用`format`参数来指定特定的格式。例如，如果我们希望输出`"03/09/2020"`这样的格式，可以将代码修改为如下：

```Gleam
let formatted_date = string.from_date(today, format = "%d/%m/%Y")
```

这里，`%d`代表日期（日），`%m`代表月份，`%Y`代表年份。更多关于格式化的信息可以参考Gleam官方文档。

## 深入了解

在Gleam中，日期和时间数据都是以`Datetime`类型来表示的，这是一个由日期、时间和时区构成的复合类型。当我们将`Datetime`类型的数据转换为字符串时，Gleam实际上是在使用`Datetime.to_string`函数，它接收一个`Datetime`类型的参数并返回一个字符串。因此，我们也可以直接调用`Datetime.to_string`来将日期转换为字符串，如下所示：

```Gleam
let today = date()
let formatted_date = Datetime.to_string(today)
```

另外，如果我们希望将一个特定的字符串解析为日期对象，也可以使用`Datetime.from_string`函数来实现。它接收一个字符串和一个格式参数，并返回一个`Datetime`类型的值。例如，如果我们有一个格式为`"03/09/2020"`的日期字符串，可以使用如下代码来将其解析为日期对象：

```Gleam
let str = "03/09/2020"
let parsed_date = Datetime.from_string(str, format = "%d/%m/%Y")
```

具体的日期解析规则也可以在Gleam官方文档中找到。

## 参考文献

- Gleam官方文档：https://gleam.run/
- 关于日期的格式化规则：https://gleam.run/documentation/standard_library/string.html#formatting-datetime-strings
- 关于日期的解析规则：https://gleam.run/documentation/standard_library/datetime.html#from-and-to-strings
- 有关使用Gleam进行日期处理的示例代码：https://github.com/search?q=language%3Agleam+datetime&type=Repositories
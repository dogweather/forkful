---
title:    "Gleam: 将日期转换为字符串"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么 ##
日期和字符串是编程中常见的数据类型，将日期转换为字符串可以让我们更方便地处理和显示日期数据。Gleam为我们提供了方便的方法来将日期转换为字符串，让我们来学习这个功能吧！

## 如何操作 ##
在Gleam中，我们可以使用`Date.to_string`函数来将日期转换为字符串。下面是一个示例代码和输出结果：

```Gleam
import gleam/datetime.{ Date }

let date = Date.new(2021, 10, 15)
let date_str = Date.to_string(date)
```

输出结果：

```Gleam
"2021-10-15"
```

## 深入探讨 ##
在Gleam中，日期和时间被抽象为`Date`和`Time`这两个数据类型。我们可以使用`Date.to_string`函数来将`Date`类型转换为字符串，也可以使用`Date.from_string`函数来将字符串转换为`Date`类型。这些转换函数遵循ISO 8601标准来表示日期和时间，确保了这些数据的统一性和可读性。

除了日期格式外，我们还可以通过在`Date.to_string`函数中传入第二个参数来指定输出字符串的格式，以满足不同的需求。例如，如果我们想要将日期以中文的形式显示，可以使用`Date.to_string(date, "yyyy年MM月dd日")`来指定输出格式。

## 参考链接 ##
- [Gleam官方文档 - 日期和时间](https://gleam.run/book/core-modules#date-and-time)
- [ISO 8601标准](https://en.wikipedia.org/wiki/ISO_8601)
- [一起学Gleam - 格式化日期](https://docs.likejazz.com/gleam/format-datetime/)
- [Gleam社区论坛](https://forum.gleam.run/)  

## 参见 ##
- [Gleam官方文档 - 字符串](https://gleam.run/book/core-modules#string)
- [Gleam官方文档 - 文件和IO](https://gleam.run/book/core-modules#file-and-io)
---
title:    "Gleam: 将日期转换为字符串"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

为什么：将日期转换为字符串是非常常见的一个需求，因为在应用程序中，我们经常需要将日期以可读性好的字符串形式展现给用户。例如，在日历应用中，我们通常会将日期以 "月-日-年" 的格式呈现给用户。

如何：为了将日期转换为字符串，我们可以使用 Gleam 标准库中的 "Date" 和 "Format" 模块。首先，我们需要先从日期类型中提取出我们需要的信息，比如月份、日期和年份。然后，我们可以使用 "Format.format" 函数来将这些信息以我们想要的字符串格式进行组合。下面是一个示例代码：

```Gleam
import Date
import Format

let date = Date.from_posix({ seconds = 1628146200 })

let month = Format.from_posix(date, "%m")
let day = Format.from_posix(date, "%d")
let year = Format.from_posix(date, "%Y")

let string_date = Format.format("#{month}-#{day}-#{year}")
```

以上代码的输出将会是 "08-05-2021"，对应了我们提取出的月份、日期和年份信息。

深入探讨：除了上面提到的基本用法之外，Gleam 还提供了许多其他的日期格式化选项。比如，我们可以设置日期的时区、时钟格式、以及添加其他文本内容等。我们还可以在 `Format.format` 函数中使用占位符 `#{}` 来动态设置日期的不同部分。通过这些方法，我们可以更加灵活地将日期转换为不同的字符串格式。

另外，Gleam 也提供了 `Date.to_iso8601_string` 函数来将日期转换为 ISO 8601 标准的字符串格式，这在某些场景下也非常实用。

请参考下面的链接来获取更多关于 Gleam 中日期转换的信息：

## 参考链接

- [Gleam 标准库中的日期处理模块](https://gleam.run/modules/date.html)
- [Format.format 函数的文档](https://gleam.run/modules/format.html#format:%20String.now%20-%3E%20String(FORMAT))
- [Gleam 社区中关于日期转换的讨论](https://github.com/gleam-lang/gleam/discussions/919)
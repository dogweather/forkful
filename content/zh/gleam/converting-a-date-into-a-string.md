---
title:                "将日期转换为字符串"
html_title:           "Bash: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 什么和为什么？

将日期转化为字符串即表示我们把日期按某种特定的格式转换成了由字符组成的文本形式。程序员这样做是因为它使信息更易于显示和理解，也方便了与其他系统之间的数据交换。

## 如何：

你可以使用以下的Gleam编程代码来转换日期为字符串：

```Gleam
import gleam/datetime.{Datetime}
import gleam/bit_builder.{BitBuilder}

fn datetime_to_string(datetime: Datetime) -> String {
    let BitBuilder = 
    |bit_builder|
    |> bit_builder.append_int(..datetime.year, 4)
    |> bit_builder.append_string("-")
    |> bit_builder.append_int(..datetime.month, 2)
    |> bit_builder.append_string("-")
    |> bit_builder.append_int(..datetime.day, 2)
    
    bit_builder.into_string()
}
```
样例输出：

```Gleam
let date = Datetime(year: 2022, month: 5, day: 20, hour: 14, minute: 30, second: 45)
datetime_to_string(date) // "2022-05-20"
```

## 深入研究：

将日期转换成字符串的方法在编程历史的发展中一直在持续的演变。在早期的编程语言中，程序员们需要自己手动完成这个转换过程，这不仅繁琐且易产生错误。因此在现代编程语言如 Gleam 中，这一过程已经大大简化和标准化。

你也可以使用其他方法来完成这个转换，例如使用某些内置函数，或者根据个别需要编写定制化的转换函数。

实现日期转字符串的过程中，我们通过拼接年月日三者和便于阅读理解的间隔符来完成，这样即实现了信息的直观传递也方便进行进一步操作。

## 参照其他资源：

你可以通过以下链接来学习更多有关字符串和日期的知识和操作：

- Gleam中细节处理模块bit_builder的链接: [https://hexdocs.pm/gleam_stdlib/gleam/bit_builder.html](https://hexdocs.pm/gleam_stdlib/gleam/bit_builder.html)
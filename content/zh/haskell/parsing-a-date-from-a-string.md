---
title:                "从字符串解析日期"
html_title:           "C: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 什么与为什么？
将字符串解析为日期是一种从中读取特定日期的方法。程序员之所以进行此操作，是因为它使我们能够有效处理和操作日期数据。

## 怎么做：
Haskell中的日期解析可以使用`Data.Time`库完成。以下是一些示例代码和输出：

```Haskell
import Data.Time

main = do
  let dateString = "2020-05-20"
  let maybeDay = parseTimeM True defaultTimeLocale "%Y-%m-%d" dateString :: Maybe Day
  print maybeDay
```
当你运行这段代码，输出应该是：

```Haskell
Just 2020-05-20
```
理想情况下，您得到一个`Just Day`类型的值，其中`Day`包含解析出的日期。如果解析失败，将返回`Nothing`。

## 深入剖析：
1. *历史背景*： 字符串解析在计算机领域有长久的历史，早在使用键盘输入日期或时间之前，程序员就开始使用它来读取日期和时间。
2. *替代方法*： 如果你对使用`Data.Time`库有困难，还可以考虑使用`Data.Time.Format.Parse`库，这个库提供了更详细的解析选项。
3. *实现细节*：解析字符串为日期时，首先需要一个日期格式，例如`"%Y-%m-%d"`。然后，`parseTimeM`函数尝试匹配字符串中的模式，并生成日期。

## 参考链接：
- 官方Haskell文档：[Data.Time](http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- Haskell解析日期教程： [Practical Haskell - Parsing Date and Time](http://www.yellosoft.us/practical-haskell/parsing-date-and-time)
- `Data.Time.Format.Parse`库的GitHub页面：[Data.Time.Format.Parse](https://hackage.haskell.org/package/time-1.10/docs/Data-Time-Format-Parse.html)
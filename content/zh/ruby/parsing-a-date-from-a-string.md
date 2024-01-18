---
title:                "从字符串解析日期"
html_title:           "Ruby: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 什么是日期解析 & 为什么要做它?

日期解析是通过编程代码从一个字符串中提取日期信息的过程。程序员通常会这样做是因为日期信息可能以不同的格式出现，如"12月25日"、"25/12"或"2020/12/25"，所以需要通过解析来标准化日期信息，以便后续处理。

## 如何实现:

```Ruby
require 'date'

date_str = "12月25日"
date = Date.parse(date_str)

puts date # 2021-12-25
```

```Ruby
require 'date'

date_str = "25/12"
date = Date.strptime(date_str, "%d/%m")

puts date # 2021-12-25
```

## 深入探讨:

在过去，日期解析是一个复杂而繁琐的过程，需要手动处理各种日期格式。但是，现在有许多库和类可以帮助我们轻松解析和处理日期信息，如Ruby标准库中的Date类和DateTime类。此外，也可以使用正则表达式来实现日期解析。在解析日期时，需要注意不同语言和文化背景所使用的日期格式，以免出现错误解析。

## 链接参考:

- Ruby官方文档：https://ruby-doc.org/stdlib/libdoc/date/rdoc/index.html
- 正则表达式介绍：https://www.runoob.com/regexp/regexp-syntax.html
- 关于日期解析的更多讨论：https://stackoverflow.com/questions/25235061/parsing-date-in-ruby
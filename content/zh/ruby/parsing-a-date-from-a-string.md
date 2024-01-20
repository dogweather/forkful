---
title:                "从字符串解析日期"
html_title:           "C: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么? (What & Why?)

解析字符串中的日期是指从字符串中提取并理解日期信息的过程。程序员这么做的目的是为了处理、格式化以及比较日期，这通常在数据处理和分析中十分常见。

## 如何操作? (How to?)

Ruby内置了解析日期的功能，只需要引用 `Date` 类即可:

```Ruby
require 'date'

str = '2021-08-13'
date = Date.parse(str)
puts date
```

运行上述脚本，你将看到如下输出：

```
2021-08-13
```

如果日期格式复杂，我们可以使用 `strptime` 方法:

```Ruby
str = '13-八月-2021'
date = Date.strptime(str, '%d-%B-%Y')
puts date
```

输出:

```
2021-08-13
```

## 深入探讨 (Deep Dive)

在早期的编程语言中，并没有内置解析日期的功能，程序员需要自己实现。Ruby中的 `Date` 类就源于此，它引用了strptime来解析复杂的日期格式。

一个可选的方式是使用 `Time` 类，但是它通常用于处理带时间的日期。而对于只有日期的情况，我们更倾向于使用`Date` 类。

实现细节上，`Date.parse` 使用的是国际日期格式（YYYY-MM-DD）。在Ruby中，对日期的解析始终依赖于语言或环境的日期格式。

## 图示参考 (See Also)

若希望了解更多关于Ruby日期解析的内容，可以参考以下链接:

- [官方文档: Date](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html)
- [官方文档: Time](https://ruby-doc.org/core-3.0.0/Time.html)
- [Ruby日期格式化指南](https://www.justinweiss.com/articles/3-ways-to-monkey-patch-without-making-a-mess/)
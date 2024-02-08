---
title:                "比较两个日期"
aliases:
- zh/ruby/comparing-two-dates.md
date:                  2024-01-20T17:33:38.397267-07:00
model:                 gpt-4-1106-preview
simple_title:         "比较两个日期"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)

比较两个日期就是确定它们谁早谁晚或者它们是否相同。程序员经常这么做来处理日志, 事件, 或者定时任务。

## How to: (如何实现：)

在Ruby中，使用`Date`类来比较日期很直接。这里有个例子：

```Ruby
require 'date'

date1 = Date.new(2023, 4, 1)
date2 = Date.new(2023, 4, 15)

puts date1 < date2   # 输出 true
puts date1 > date2   # 输出 false
puts date1 == date2  # 输出 false
```

比较日期时，结果将是布尔值，即：`true`或`false`。

## Deep Dive (深入探索)

Ruby使用`Date`类来处理日期。这个类是1970年代末到1980年代初UNIX时间的一个产物。不同的是，`Date`类不是从1970年1月1日开始所谓的'Epoch'，它能处理更早以及更晚的日期。

另外，你可能会用到`Time`类来处理具体时间点，但是请注意，`Time`对处理早于Epoch的日期会有问题。

最后，比较两个日期时，Ruby实际上在比较两个日期的内部整数表示，这是一个高效的操作。

## See Also (另请参阅)

- [Time类文档](https://ruby-doc.org/core-2.7.0/Time.html)
- Stack Overflow 上关于[Ruby日期比较](https://stackoverflow.com/questions/tagged/ruby+date+comparison)的讨论

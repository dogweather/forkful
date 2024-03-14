---
date: 2024-01-20 17:33:38.397267-07:00
description: "\u6BD4\u8F83\u4E24\u4E2A\u65E5\u671F\u5C31\u662F\u786E\u5B9A\u5B83\u4EEC\
  \u8C01\u65E9\u8C01\u665A\u6216\u8005\u5B83\u4EEC\u662F\u5426\u76F8\u540C\u3002\u7A0B\
  \u5E8F\u5458\u7ECF\u5E38\u8FD9\u4E48\u505A\u6765\u5904\u7406\u65E5\u5FD7, \u4E8B\
  \u4EF6, \u6216\u8005\u5B9A\u65F6\u4EFB\u52A1\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.387548-06:00'
model: gpt-4-1106-preview
summary: "\u6BD4\u8F83\u4E24\u4E2A\u65E5\u671F\u5C31\u662F\u786E\u5B9A\u5B83\u4EEC\
  \u8C01\u65E9\u8C01\u665A\u6216\u8005\u5B83\u4EEC\u662F\u5426\u76F8\u540C\u3002\u7A0B\
  \u5E8F\u5458\u7ECF\u5E38\u8FD9\u4E48\u505A\u6765\u5904\u7406\u65E5\u5FD7, \u4E8B\
  \u4EF6, \u6216\u8005\u5B9A\u65F6\u4EFB\u52A1\u3002"
title: "\u6BD4\u8F83\u4E24\u4E2A\u65E5\u671F"
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

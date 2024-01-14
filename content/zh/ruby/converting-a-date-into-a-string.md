---
title:    "Ruby: 将日期转换为字符串"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么

为什么有时候我们需要将日期转换成字符串？事实上，这是一种很常见的需求，因为我们通常会在我们的程序中显示日期，但是日期对象本身是无法直接显示在屏幕上的。因此，我们需要将它们转换成字符串，以方便我们在程序中使用。

## 如何做

下面将介绍如何使用Ruby将日期转换成字符串。首先，我们需要使用```.to_s```方法将日期对象转换成字符串。让我们看一个例子：

```Ruby 
date = Date.today
puts date.to_s
```

输出应该为：```2021-08-24```

我们也可以指定日期的格式，例如使用```("%m/%d/%Y")```将日期转换成美国的日期格式：

```Ruby
date = Date.today
puts date.strftime("%m/%d/%Y")
```

输出应该为：```08/24/2021```

要了解更多关于可用的日期格式，请查阅Ruby文档。

## 深入探讨

日期对象和字符串之间的转换实际上是通过使用日期类的实例方法来实现的。这些方法包括```.to_s```和```.strftime```。使用这些方法，我们可以在我们的程序中轻松地操作日期对象，使它们适合我们的需求。

另外，我们也可以使用```Date.strptime```方法将字符串转换成日期对象。这样可以方便我们从用户输入的字符串中提取出日期信息。

## 参考链接

- [Ruby Date类文档](http://ruby-doc.org/stdlib-2.1.1/libdoc/date/rdoc/Date.html)
- [Ruby strftime方法文档](http://ruby-doc.org/core-2.0.0/Time.html#method-i-strftime)
- [Ruby日期对象教程](https://www.geeksforgeeks.org/date-and-time-in-ruby/)

## 参见

- [如何使用Ruby获取当前日期和时间](https://example.com)
- [Ruby字符串基础知识介绍](https://example.com)
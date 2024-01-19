---
title:                "获取当前日期"
html_title:           "Arduino: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 什么和为什么？（What & Why?）

获取当前日期是获取计算机系统当前时间的过程。程序员经常需要使用到这在各种场景，如时间戳的记录，事件分析以及用户行为跟踪等。

## 如何获取当前日期：（How to:）

直接使用 `Time.now` 就可以获取到准确的当前时间和日期。来看看如何使用。
```Ruby
现在 = Time.now
puts 现在
```
运行上述程序，你将得到如下输出：
```
2022-05-23 10:34:45 +0800
```

## 深入浅出（Deep Dive）

获取当前日期作为一种基本功能，在Ruby以及大多数其他编程语言中均有实现。它的用途广泛，比如在日志跟踪中常常用这来创建时间戳，也可以用来分析和跟踪用户的行为模式等。对于Ruby而言，`Time.now` 就是实现这个功能的主要方法。除此之外，你还可以使用 `DateTime.now` 获取相同的结果。

```Ruby
现在 = DateTime.now
puts 现在
```
这将得到相似的结果。

取决于你的需求，你可能需要更详细的日期和时间信息，Ruby的 Time 和 DateTime 类提供了一些其他方法，如 `year`, `month`, `day`, `hour`, `min`, `sec`，来获取更详细的信息。

## 延伸阅读：（See Also）

Ruby文档中关于Time类的更多信息：https://docs.ruby-lang.org/en/2.7.0/Time.html

关于DateTime类的更多信息：https://docs.ruby-lang.org/en/2.7.0/DateTime.html

你也可以参考这篇文章来更深入理解Ruby中的日期和时间：https://www.jianshu.com/p/aedf422ec2ab
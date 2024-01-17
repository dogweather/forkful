---
title:                "计算未来或过去的日期"
html_title:           "Ruby: 计算未来或过去的日期"
simple_title:         "计算未来或过去的日期"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

#在未来或过去计算日期的原因及方法

## 什么是计算未来或过去日期？为什么程序员要这样做？

计算日期是指以特定的间隔从当前日期往前或往后推算一个新日期。程序员经常需要计算日期，例如在编写软件时需要向用户显示特定日期的信息。此外，还可以用于编写日历或计划应用程序。

## 如何进行计算？

```Ruby
# 计算未来日期
Date.today + 7 # 加 7 天
#=> 2021-09-26

Date.today + (30 * 3) # 加 90 天
#=> 2021-12-25

Date.today - 14 # 减 14 天
#=> 2021-09-05

# 计算过去日期
Date.today - 7 # 减 7 天
#=> 2021-09-12

Date.today - (30 * 3) # 减 90 天
#=> 2021-06-16

Date.today + 14 # 加 14 天
#=> 2021-09-29
```

## 深入了解

计算日期的历史可以追溯到古代，人们使用日历来记录时间。另外，除了使用Ruby中的简便方法外，还可以使用时间戳或UNIX时间来计算日期。此外，还可以使用其他编程语言实现类似的功能，例如Python的datetime模块。

## 相关资源

- [Ruby官方文档](https://www.ruby-lang.org/zh_cn/)
- [关于日期计算的教程](https://www.rubyguides.com/2015/08/ruby-date-and-time/)
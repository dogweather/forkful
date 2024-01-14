---
title:                "Fish Shell: 计算未来或过去的日期"
simple_title:         "计算未来或过去的日期"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 为什么

在日常生活和工作中，我们经常需要计算未来或过去的日期，比如预订旅行，安排会议，或者跟踪过去的事件。使用Fish Shell编程可以帮助我们轻松地进行日期计算，提高我们的工作效率。

# 如何实现

```Fish Shell```中有一个非常方便的命令可以帮助我们计算未来或过去的日期，即```date```命令。要计算未来的日期，我们可以在命令后加上```next```关键词，然后再加上要计算的日期，比如```date next Monday```。如果要计算过去的日期，则加上```last```关键词，比如```date last Friday```。下面是几个示例及其对应的输出：

```Fish Shell
date next Sunday
Sun Oct 6 00:00:00 UTC 2019

date last Wednesday
Wed Sep 25 00:00:00 UTC 2019

date next month
Fri Nov  1 00:00:00 UTC 2019
```

# 深入了解

除了使用```next```和```last```关键词外，我们还可以使用更多的参数来精确计算日期。比如，我们可以使用```-d```参数来指定一个基准日期，比如```date -d "2019-09-30" next Monday```，这样就可以在指定日期的基础上计算未来的周一日期。我们还可以使用```-v```参数来增加或减少特定的时间单位，比如```date -v+3d```会计算三天后的日期。更多命令的使用方法和参数可以在```Fish Shell```的官方文档中找到。

# 参考链接

- [Official Fish Shell documentation](https://fishshell.com/docs/current/index.html)
- [Fish Shell date command](https://fishshell.com/docs/current/commands.html#date)
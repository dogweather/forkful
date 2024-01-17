---
title:                "获取当前日期"
html_title:           "PHP: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

今天我们来讨论一下如何在PHP中获取当前的时间。获取当前时间可以帮助我们在网站或应用程序中显示正确的时间信息，也可以用于记录用户操作的时间戳。接下来，我们将介绍如何使用PHP代码来获取当前日期，并深入了解这个功能的历史背景和其他替代方案。

## 什么是当前日期？为什么程序员需要它？

当前日期就是现在的日期，通常包括年、月、日、时、分、秒等信息。程序员常常需要获取当前日期，因为它可以帮助我们跟踪时间信息，比如记录用户何时注册或登陆。此外，许多网站和应用程序需要显示当前日期，以便提供实时信息。

## 如何使用PHP获取当前日期？

在PHP中，获取当前日期非常简单。我们可以使用内置的函数```date()```来获取当前日期。以下是一些示例代码：

```
// 获取当前日期
$current_date = date('Y-m-d');

// 获取当前日期和时间
$current_date_time = date('Y-m-d H:i:s');

// 获取当前月份
$current_month = date('F');

// 获取当前年份
$current_year = date('Y');
```

以上代码使用了```date()```函数，并传入参数来指定日期的格式。您可以根据自己的需要调整日期格式。以下是部分可能用到的参数：

- Y年（四位数）
- m月（01-12）
- d日（01-31）
- H小时（00-23）
- i分钟（00-59）
- s秒（00-59）
- F月份全名（比如January, February等）

##深入了解

历史背景：PHP的日期和时间函数从PHP3版本开始就存在了。最初，PHP使用C语言的时间和日期库来支持这些功能。在后续版本中，PHP逐渐扩展了这些功能，提供更多参数和格式选项。

替代方案：除了使用PHP内置的日期和时间函数，也可以使用第三方库来获取当前日期。比如，Carbon是一个流行的PHP日期和时间库，它提供了更多便捷的方法来处理日期和时间。

实现详情：如果您想要更深入地了解如何在PHP中获取当前日期，可以查看PHP官方文档中有关```date()```函数的详细信息。您也可以查看PHP源码来了解日期和时间功能的具体实现原理。

## 更多相关资源

- PHP官方文档：https://www.php.net/manual/en/function.date.php
- Carbon官方网站：https://carbon.nesbot.com/
- PHP源码：https://github.com/php/php-src
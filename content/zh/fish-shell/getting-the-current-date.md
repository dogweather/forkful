---
title:                "获取当前日期"
date:                  2024-01-20T15:14:08.644441-07:00
html_title:           "Bash: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么?)
获取当前日期是一个查看或者使用系统日期和时间的动作。程序员经常需要这样做来记录日志、生成时间戳或为功能比如定时任务提供支持。

## How to (怎么做)
在Fish Shell里，你可以用内置的`date`命令获取当前日期。直接敲入`date`，就会输出当前的日期和时间。

```Fish Shell
date
```

输出可能会像这样：

```
周三 三月  8 12:34:56 CST 2023
```

想要自定义日期格式，可以加上`+%Y-%m-%d`这样的参数。

```Fish Shell
date "+%Y-%m-%d"
```

输出：

```
2023-03-08
```

## Deep Dive (深入了解)
在UNIX系统和类UNIX系统中，获取当前日期的指令由来已久，基本每个系统都支持。Fish Shell自带`date`命令，其实是调用了底层系统的提供的`date`功能。

其他Shell, 比如Bash和Zsh, 也有类似的功能，但Fish的设计更简单、更人性化。在使用上，尽管不同Shell的命令参数可能小有不同，核心是相通的。

获取当前日期的实现细节，主要是调用了操作系统底层的时间和日期服务。操作系统通常会使用硬件时钟维护时间，并提供系统调用供不同应用和服务读取和设置时间。

## See Also (另见)
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html) - 了解更多关于Fish Shell的信息。

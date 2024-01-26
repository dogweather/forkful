---
title:                "日志记录"
date:                  2024-01-26T01:00:43.501759-07:00
model:                 gpt-4-1106-preview
simple_title:         "日志记录"
programming_language: "C"
category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/logging.md"
---

{{< edit_this_page >}}

## 什么与为什么？
日志记录本质上是记录下你的程序在做什么，通常是通过将消息写入文件或终端来实现的。程序员这么做是为了跟踪事件，诊断问题，以及拥有一个审计轨迹，讲述应用程序运行的历程。

## 如何操作：
让我们从一些基础开始。C语言没有内建的日志框架，但是你可以用`stdio.h`来实现一些简单的东西。方法如下：

```c
#include <stdio.h>
#include <time.h>

void logMessage(const char* message) {
    time_t now;
    time(&now);
    char *date = ctime(&now);
    date[strlen(date) - 1] = '\0'; // 去除ctime()结果末尾的换行符
    printf("[%s] %s\n", date, message);
}

int main() {
    logMessage("应用程序已启动。");
    // ... 你的代码写在这里 ...
    logMessage("应用程序正在执行重要操作。");
    // ... 代码继续 ...
    logMessage("应用程序已结束。");
    return 0;
}
```

样本输出可能看起来像这样：

```
[周二 3月 9 12:00:01 2023] 应用程序已启动。
[周二 3月 9 12:00:02 2023] 应用程序正在执行重要操作。
[周二 3月 9 12:00:03 2023] 应用程序已结束。
```

当然，在现实世界中，你可能希望写入文件而不是终端，处理不同的日志级别，并可能使用预定义的库。

## 深入研究
在C语言中的日志记录有着古朴的魅力——它和语言的大多数其他部分一样低级。从历史上看，日志记录是使用`fprintf`与`stderr`或文件指针来执行的。随着程序变得更加复杂，日志记录的需求也增长了，这导致了如Unix系统上的`syslog`这类库的发展，它能够处理来自多个源的不同重要级别的日志。

在现代环境中，有许多C语言日志库可用，例如`zlog`、`log4c`和`glog`，它们提供丰富的功能集，包括日志轮换、结构化日志记录和多线程日志记录。这些解决方案允许对日志的冗余度、目的地和格式进行细粒度控制。

在实现日志系统时，需要考虑时间戳格式化、日志文件管理和性能等细节。时间戳日志对于事件相关性是至关重要的，而日志轮换确保日志文件不会占用太多磁盘空间。日志记录行为也应该是快速且不阻塞主应用流程的，以防日志记录成为瓶颈。

## 另见
要更深入地了解C语言中的日志库和实践，请查看以下资源：

- GNU `syslog`手册：https://www.gnu.org/software/libc/manual/html_node/Syslog.html
- `zlog`：一个高度可配置的C语言日志库 - https://github.com/HardySimpson/zlog
- `log4c`：一个仿照Log4j的C语言日志框架 - http://log4c.sourceforge.net/
- `glog`：谷歌的应用级日志库 - https://github.com/google/glog
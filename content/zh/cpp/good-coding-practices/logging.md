---
title:                "日志记录"
aliases: - /zh/cpp/logging.md
date:                  2024-01-26T01:00:20.779853-07:00
model:                 gpt-4-1106-preview
simple_title:         "日志记录"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/logging.md"
---

{{< edit_this_page >}}

## 什么和为什么？
在编程语境中，日志记录是指将事件、状态和信息记录到文件或其他输出媒介的过程。程序员进行日志记录是为了追踪他们的应用程序中发生的情况，调试问题，以及为未来的分析和优化监控性能。

## 如何操作：
假设你在Linux系统上工作，想要使用好老的C++语言将你的日志信息丢进一个文件。你需要包含`<iostream>`和`<fstream>`库来执行文件操作。这里有一个简短的例子：

```C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ofstream logFile("appLog.txt", std::ios::app);  // 以追加模式打开

    if (!logFile.is_open()) {
        std::cerr << "打开日志文件出现问题！" << std::endl;
        return 1;
    }

    logFile << "应用程序启动" << std::endl;
  
    // ... 应用逻辑中的某个地方
    logFile << "发生了一个重要事件" << std::endl;

    // 不要忘记关闭你的文件流
    logFile.close();

    return 0;
}
```

如果你使用`tail -f appLog.txt`命令来查看你的日志文件，你应该会看到：

```
应用程序启动
发生了一个重要事件
```

整洁，你已经得到了一个带时间戳的事件记录！

## 深入探讨
日志记录与计算本身一样古老，其根源在于在纸上留下标记，以追踪古代计算机的活动。在现代时代，它全部关于复杂的软件解决方案。你可以直接进行文件日志记录，就像上面的快速简陋示例，或者你可能会沉迷于一个更高级的日志记录框架，如C++领域的Log4cpp或Boost.Log；这些强悍的工具提供日志级别、格式控制等功能。

说到级别，日志记录的最佳实践包括使用不同严重性级别的日志——信息、调试、警告、错误、致命——这样当你试图解决问题或弄明白你的应用为什么像个情绪化的青少年一样行为时，你可以过滤掉杂音。

在性能方面，不要在你的日志上偷懒。过量的日志记录会让你快速的应用变成蜗牛马拉松，拖慢文件系统，或者如果你是基于云的，甚至会因存储费用而花掉你的钱。关键是要找到正确的平衡：记录你所需要的，不多也不少。

## 另请参阅
对于那些喜欢在日志记录实践上走额外一英里的人，请查看：

- [Boost.Log库](https://www.boost.org/doc/libs/1_75_0/libs/log/doc/html/index.html)，它提供了一些重量级的日志功能。
- [Google的glog库](https://github.com/google/glog)，如果你对科技巨头的厨师们使用什么来记录他们的应用程序感兴趣的话。
- [Log4cpp库](http://log4cpp.sourceforge.net/)，它提供了一个可配置的日志机制。

对于日志记录的原因和方法的背景阅读，深入了解：

- 这个Stack Overflow关于[日志记录最佳实践](https://stackoverflow.com/questions/783956/logging-best-practices)的帖子会给你一个同行评审的深入探讨该主题。

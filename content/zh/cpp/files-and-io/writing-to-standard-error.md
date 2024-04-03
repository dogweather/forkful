---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:43.915678-07:00
description: "\u5728 C++ \u4E2D\u5199\u5165\u6807\u51C6\u9519\u8BEF\uFF08`stderr`\uFF09\
  \u6D89\u53CA\u8F93\u51FA\u4E0E\u4E3B\u7A0B\u5E8F\u8F93\u51FA\u5206\u5F00\u7684\u9519\
  \u8BEF\u6D88\u606F\u6216\u8BCA\u65AD\u4FE1\u606F\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\
  \u505A\u662F\u4E3A\u4E86\u5C06\u9519\u8BEF\u5F15\u5BFC\u81F3\u4E0D\u540C\u7684\u6D41\
  \uFF0C\u901A\u8FC7\u533A\u5206\u6B63\u5E38\u8F93\u51FA\u548C\u9519\u8BEF\u6D88\u606F\
  \uFF0C\u4F7F\u5F97\u8C03\u8BD5\u548C\u9519\u8BEF\u5904\u7406\u66F4\u52A0\u5BB9\u6613\
  \u3002"
lastmod: '2024-03-13T22:44:48.128492-06:00'
model: gpt-4-0125-preview
summary: "\u5728 C++ \u4E2D\u5199\u5165\u6807\u51C6\u9519\u8BEF\uFF08`stderr`\uFF09\
  \u6D89\u53CA\u8F93\u51FA\u4E0E\u4E3B\u7A0B\u5E8F\u8F93\u51FA\u5206\u5F00\u7684\u9519\
  \u8BEF\u6D88\u606F\u6216\u8BCA\u65AD\u4FE1\u606F\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\
  \u505A\u662F\u4E3A\u4E86\u5C06\u9519\u8BEF\u5F15\u5BFC\u81F3\u4E0D\u540C\u7684\u6D41\
  \uFF0C\u901A\u8FC7\u533A\u5206\u6B63\u5E38\u8F93\u51FA\u548C\u9519\u8BEF\u6D88\u606F\
  \uFF0C\u4F7F\u5F97\u8C03\u8BD5\u548C\u9519\u8BEF\u5904\u7406\u66F4\u52A0\u5BB9\u6613\
  \u3002."
title: "\u5199\u5165\u6807\u51C6\u9519\u8BEF"
weight: 25
---

## 什么和为什么？

在 C++ 中写入标准错误（`stderr`）涉及输出与主程序输出分开的错误消息或诊断信息。程序员这样做是为了将错误引导至不同的流，通过区分正常输出和错误消息，使得调试和错误处理更加容易。

## 如何操作：

在 C++ 中，可以通过使用标准库中的 `cerr` 流来实现写入标准错误。这里有一个基本示例：

```cpp
#include <iostream>

int main() {
    // 写入标准输出
    std::cout << "这是一条普通消息。" << std::endl;
    
    // 写入标准错误
    std::cerr << "这是一条错误消息。" << std::endl;
    
    return 0;
}
```

示例输出：
```
这是一条普通消息。
这是一条错误消息。
```

在这个例子中，两条消息通常会在终端上显示，但你可以在 shell 中分别重定向它们。例如，你可以将标准输出发送到一个文件，同时让错误显示在屏幕上。

对于更高级的日志和错误处理，可以使用第三方库，如 `spdlog` 或 `boost.log`。这些库为日志记录提供了增强的功能，包括格式化、日志级别和文件输出。

这里展示了如何使用 `spdlog` 来写入一条错误消息：

```cpp
#include "spdlog/spdlog.h"

int main() {
    // 初始化 spdlog
    spdlog::info("这是一条普通消息。");
    spdlog::error("这是一条错误消息。");
    
    return 0;
}
```

注意：要使用 `spdlog`，你需要将其添加到你的项目中。你可以通过从 GitHub 克隆仓库或使用包管理器如 `vcpkg` 或 `conan` 来完成此操作。

记住，直接使用标准流或如 `spdlog` 这样的库之间的选择，取决于你的应用程序的复杂性和你对错误处理及日志的具体需求。

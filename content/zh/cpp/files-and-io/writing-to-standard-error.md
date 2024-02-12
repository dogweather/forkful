---
title:                "写入标准错误"
date:                  2024-02-03T19:32:43.915678-07:00
model:                 gpt-4-0125-preview
simple_title:         "写入标准错误"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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

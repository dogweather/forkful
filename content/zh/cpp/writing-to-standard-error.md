---
title:                "写入标准错误"
date:                  2024-01-19
simple_title:         "写入标准错误"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 写入标准错误：何为何用？
在C++编程中，写入标准错误(`stderr`)让你可以将错误信息与常规程序输出分开。这样能够方便地监控和记录程序运行时的问题。

# 操作指南：
下面的代码展示了如何写入标准错误：

```C++
#include <iostream>

int main() {
    std::cerr << "发生了一个错误！" << std::endl;
    return 0;
}
```

输出样例：
```
发生了一个错误！
```

# 深入探讨：
标准错误流(`stderr`)的概念来源于早期Unix哲学，目的是让错误信息可以被重定向或独立处理。作为标准输出(`stdout`)的一个重要补充，其允许错误信息即使在输出被重定向时也能被看到或存储。作为替代，你可以使用日志库写入日志文件。在C++中，标准库提供了`std::cerr`对象，它默认连接到标准错误流，并且是非缓冲的，这意味着信息会立即输出。

# 延伸阅读：
- C++标准库参考资料：http://www.cplusplus.com/reference/iostream/cerr/
- Unix编程艺术，讨论标准流：https://en.wikipedia.org/wiki/Unix_philosophy
- C++日志库Boost.Log：https://www.boost.org/doc/libs/1_75_0/libs/log/doc/html/index.html

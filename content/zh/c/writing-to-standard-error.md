---
title:                "写入标准错误"
html_title:           "Arduino: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么？)
在C语言中，将信息写入标准错误（stderr）用于显示错误和日志。这样做是因为可以将错误信息与标准输出（stdout）分开，便于调试和日志记录。

## How to: (怎么做：)
```C
#include <stdio.h>

int main() {
    fprintf(stderr, "发生错误：文件未找到\n");
    // 做其他工作
    return 0;
}
```
输出示例：
```
发生错误：文件未找到
```

## Deep Dive (深入探究)
- 历史背景：`stderr`与UNIX和C语言密切相关，它自1970年代初始就被设计用来区分不同的输出流。
- 替代方法：可以使用`perror()`来打印错误信息，它会自动将输出定向到`stderr`。
- 实现细节：`stderr`不会像`stdout`那样默认被缓冲，这意味着信息会直接写入而不存储在缓冲区中。

## See Also (另请参阅)
- C标准库 - `<stdio.h>`: [https://en.cppreference.com/w/c/io](https://en.cppreference.com/w/c/io)
- UNIX系统编程: [https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html](https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html)
- 命令行使用: [https://www.gnu.org/software/libc/manual/html_node/Pipe-to-a-Subprocess.html](https://www.gnu.org/software/libc/manual/html_node/Pipe-to-a-Subprocess.html)
- 深入理解计算机系统 (书籍推荐)

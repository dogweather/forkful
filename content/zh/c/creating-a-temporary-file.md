---
title:                "创建临时文件"
date:                  2024-01-20T17:39:32.080221-07:00
model:                 gpt-4-1106-preview
simple_title:         "创建临时文件"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
创建临时文件，就是新建一个文件供短期使用，通常用来存储在处理过程中产生的数据。程序员这么做通常是为了保护数据不被长期存储，或者为了减少对主存储的影响。

## How to (如何实现)
```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    char temp_filename[] = "tempfile-XXXXXX";
    int fd = mkstemp(temp_filename);
    
    if (fd == -1) {
        perror("Error creating temporary file");
        return EXIT_FAILURE;
    }
    printf("Temporary file created: %s\n", temp_filename);

    // Use the file descriptor `fd` with read/write operations...

    // Close and remove the temporary file
    close(fd);
    unlink(temp_filename);

    return EXIT_SUCCESS;
}
```
临时文件创建成功，将打印：
```
Temporary file created: tempfile-123456
```

## Deep Dive (深入了解)
在 C 语言程序中创建临时文件有历史渊源。早期 `tmpnam` 和 `tempnam` 函数用来生成临时文件名，但存在安全隐患。`mkstemp` 函数后来提供了一个更安全的方式，它生成一个唯一的文件并立即打开，最小化了文件名冲突和安全风险。还有 `mkdtemp` 函数创建临时目录。

还有标准库的 `tmpfile` 函数，它可以创建一个临时的二进制文件用于读写操作，文件关闭时自动删除。但相比 `mkstemp`，它提供的控制较少。

深入来说，操作系统如何实现临时文件管理各不相同。通常，它们都会在特定目录（如 `/tmp`）下操作，有时需要考虑文件系统权限和磁盘空间限制。

## See Also (另请参阅)
- C Standard Library documentation on temporary file creation: https://en.cppreference.com/w/c/io/tmpfile
- Linux Man Pages for `mkstemp`: https://linux.die.net/man/3/mkstemp
- Computer Security considerations for temp file creation: https://owasp.org/www-community/vulnerabilities/Insecure_Temporary_File
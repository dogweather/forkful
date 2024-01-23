---
title:                "编写文本文件"
html_title:           "Arduino: 编写文本文件"
simple_title:         "编写文本文件"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
写入文本文件让你把数据存储到硬盘。程序员这么做是为了永久保存信息，便于以后读取。

## How to (如何实现)
```C
#include <stdio.h>

int main() {
    FILE *fp = fopen("example.txt", "w");

    if (fp == NULL) {
        perror("文件打开失败");
        return -1;
    }

    fprintf(fp, "Hello, World!\n");

    fclose(fp);
    return 0;
}
```
运行这段代码会在当前目录生成 `example.txt` 文件，其中包含文本 `Hello, World!`。

## Deep Dive (深入了解)
创建文本文件是C语言早期就支持的功能，用于数据存储和交换。除了 `fprintf`，还有 `fputs` 和 `fwrite` 等函数写文件。写入时应注意文件打开模式——`"w"` 会覆盖已有内容，而 `"a"` 则追加。编码格式通常为UTF-8，但也可根据需要选择其他编码。

## See Also (另请参阅)
- C标准库参考: https://en.cppreference.com/w/c/io
- 文件I/O教程: https://www.tutorialspoint.com/cprogramming/c_file_io.htm
- 关于字符编码: https://www.joelonsoftware.com/2003/10/08/the-absolute-minimum-every-software-developer-absolutely-positively-must-know-about-unicode-and-character-sets-no-excuses/

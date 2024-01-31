---
title:                "阅读文本文件"
date:                  2024-01-20T17:53:57.156607-07:00
model:                 gpt-4-1106-preview
simple_title:         "阅读文本文件"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
在C语言中读取文本文件就是将文件内容加载到程序中。程序员这么做主要是为了处理数据和配置，或者是读取用户生成的内容。

## How to: (如何操作：)
```c
#include <stdio.h>

int main() {
    FILE *file_pointer = fopen("example.txt", "r"); // 打开文件
    if(file_pointer == NULL) {
        perror("Error opening file");
        return 1;
    }
    
    // 读取文件内容
    char buffer[100];
    while(fgets(buffer, 100, file_pointer) != NULL) {
        printf("%s", buffer);
    }
    
    fclose(file_pointer); // 关闭文件
    return 0;
}
```
输出样例：
```plaintext
Hello, World!
This is a text file.
```

## Deep Dive (深入了解)
最早C语言在1972年就已经开始使用文本文件读取的功能，标准库中的`<stdio.h>`头文件就提供了这些功能。

除了`fgets`以外，还有其他几种读取文件的方式，例如`fscanf`或逐字符读取的`fgetc`。高级用法中，可能会用到`fread`，它可以按块读取文件内容，适用于非文本文件。

关于实现细节，值得注意的是要检查文件打开是否成功，并且在读取完毕后关闭文件，这样可以避免资源泄露。

## See Also (参考链接)
- C标准库文档：[cplusplus.com/reference/cstdio/](http://www.cplusplus.com/reference/cstdio/)
- C文件I/O教程：[tutorialspoint.com/cprogramming/c_file_io.htm](https://www.tutorialspoint.com/c_standard_library/c_function_fclose.htm)
- 资源管理和错误处理最佳实践讨论：[stackoverflow.com/questions/tagged/c+file-io](https://stackoverflow.com/questions/tagged/c+file-io)

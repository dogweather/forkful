---
title:                "读取文本文件"
html_title:           "C: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 什么是读取文本文件？为什么程序员要这么做？

读取文本文件是指从计算机中读取文本内容，并将其加载到程序中。程序员通常需要读取文本文件，因为它们包含了重要的数据或配置信息，这些信息可以帮助程序运行或进行数据交互。

## 如何做？
 
读取文本文件的第一步是打开文件，然后使用特定的函数来读取内容并存储在变量中。接下来，可以使用循环来逐行读取文件的内容，并使用特定的函数来处理文本。最后，文件应该被关闭以释放内存空间。

```C 
#include <stdio.h> 
  
int main() 
{ 
    // 打开文件 
    FILE* file = fopen("example.txt", "r"); 
  
    // 读取文件内容并存储在变量中 
    char content[100]; 
    fscanf(file, "%s", content); 
  
    // 逐行读取文件内容并处理文本 
    while(fgets(content, 100, file) != NULL) 
    { 
        // 在此处处理文本 
        printf("%s", content); 
    } 
  
    // 关闭文件 
    fclose(file); 
  
    return 0; 
} 
```

**输出示例：**

"Hello World!"

"Welcome to Mandarin readers, this is a sample text file for demonstration purposes."

## 深入探讨

在过去，计算机只能处理二进制数据，因此需要使用特定的程序将文本格式转换为二进制格式。然而，随着计算机技术的发展，现在几乎所有的操作系统都能够以文本形式存储和读取数据，因此这一步骤已经不再必要。

另外，除了文本文件，还有一种称为二进制文件的文件类型。与文本文件不同的是，二进制文件中存储的是机器语言指令，而非文本字符。因此，读取二进制文件需要使用不同的函数。

如果希望一次性读取整个文本文件而不是每次读取一行，可以使用```fread()```函数。此外，还可以使用```fgets()```函数读取指定数量的字符，而避免一次读取整个文件。

## 参考资料
- [C语言中文网](https://c.biancheng.net/c/)
- [C程序设计语言（第二版）-Brian W. Kernighan, Dennis M. Ritchie](https://book.douban.com/subject/1139336/)
- [C语言标准库](https://zh.cppreference.com/w/c/string/byte/fread)
---
title:    "C: 读取文本文件"
keywords: ["C"]
---

{{< edit_this_page >}}

# 为什么要阅读文本文件

阅读文本文件是编程中的一个重要技能，它可以让我们处理大量的数据并从中获取所需的信息。通过阅读文本文件，我们可以更加灵活地处理数据，而不仅仅局限于预先设置好的变量或数据类型。因此，了解如何读取文本文件对于编程学习者来说非常重要。

## 如何读取文本文件

要读取文本文件，我们需要使用C语言中的"```fopen()```"函数来打开文件，并使用"```fscanf()```"函数来从文件中读取数据。下面是一个读取文件内容并将数据打印到屏幕上的示例代码：

```C
#include <stdio.h> 
int main() 
{ 
    FILE *file; 
    int number; 
    //打开文件 
    file = fopen("sample.txt", "r"); 
    
    if(file == NULL) 
    { 
        printf("无法打开文件！"); 
        return 1; 
    } 
    
    //读取文件中的整数并打印 
    while(!feof(file)) 
    { 
        fscanf(file, "%d", &number); 
        printf("%d\n", number); 
    } 
    
    //关闭文件 
    fclose(file); 
    
    return 0; 
} 
```

假设我们的文本文件"sample.txt"中包含如下内容：

```
10
20
30
```

运行上述代码后，我们将看到如下输出：

```
10
20
30
```

## 深入探讨

用"```fopen()```"打开文件时，我们需要指定所要打开的文件的名称和打开文件的模式，其中"```"r```"代表只读模式。在使用"```fscanf()```"读取文件时，我们需要提供对应的格式说明符来读取不同类型的数据。例如"%d"用于读取整数，"%f"用于读取浮点数，"%s"用于读取字符串等。

此外，我们还可以使用"```fgets()```"函数来读取字符串，以"```getc()```"函数来读取单个字符。在处理大型文本文件时，我们可以使用循环结构与以上函数结合，一次读取并处理文件的一部分内容，以避免程序崩溃或占用过多内存。

# 参考链接

- [C语言文件操作](https://www.runoob.com/cprogramming/c-file-io.html)
- [从文件中读取数据](https://www.programiz.com/c-programming/c-file-input-output)
- [C语言文件读写操作详解](https://www.cnblogs.com/loren-Yang/articles/3733275.html)

# 请参见

- [如何写入文本文件](https://www.blog.example/write-text-file-mandarin)
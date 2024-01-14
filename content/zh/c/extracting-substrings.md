---
title:    "C: 提取子字符串"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/c/extracting-substrings.md"
---

{{< edit_this_page >}}

# 为什么要提取子串？

在C编程中，我们经常需要从字符串中提取部分内容，这就是提取子串的过程。例如，我们可能需要从一个字符串中提取出特定的单词或者一段字符。提取子串可以帮助我们更方便地处理字符串，并且提高代码的可读性和可维护性。

## 如何提取子串？

```C
#include <stdio.h>
#include <string.h>

int main()
{
    // 声明一个字符串
    char str[50] = "这是一个示例字符串";
    
    // 提取子串
    char substr[10];
    strncpy(substr, str + 4, 4); // 提取字符串中下标为4-7的字符
    substr[4] = '\0'; // 手动在末尾添加结束符
    
    // 输出结果
    printf("提取的子串为：%s", substr);
    
    return 0;
}
```

输出结果为：示例

这个例子中，我们使用了字符串函数`strncpy`来提取子串。首先，我们声明了一个字符串`str`，然后用`strncpy`函数从字符串中提取下标为4-7的字符，存放到新的字符串`substr`中。最后，我们手动添加了一个结束符以确保提取的子串正确。

## 深入了解提取子串

提取子串的过程其实就是从一个字符串中“截取”出一部分字符。在C语言中，我们可以使用`strncpy`函数来进行子串提取。这个函数有三个参数，第一个参数为目标字符串（即提取的子串），第二个参数为源字符串，第三个参数为需要提取的字符数量。值得注意的是，`strncpy`函数在提取子串时，会自动在末尾添加结束符`\0`，但是该函数会忽略`\0`，因此我们需要手动添加`\0`来确保提取的子串正确。

# 另请参阅

- [C语言字符串函数手册](https://www.runoob.com/cprogramming/c-standard-library-string-h.html)
- [C语言字符串操作教程](https://www.runoob.com/cprogramming/c-standard-library-string-operations.html)
- [C语言中的字符串提取](https://www.geeksforgeeks.org/c-program-extracting-substring-from-a-string/)
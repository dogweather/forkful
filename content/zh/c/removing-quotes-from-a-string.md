---
title:                "从字符串中移除引号"
date:                  2024-01-26T03:38:46.573905-07:00
model:                 gpt-4-0125-preview
simple_title:         "从字符串中移除引号"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## 何为 & 为何？

从字符串中移除引号意味着清除字符串内容中的任何引号标志—不论是单引号 ('') 还是双引号 ("")。程序员这样做是为了清理输入、为进一步处理数据做准备，或者在处理路径和命令时避免语法错误，这些路径和命令在使用引号标记字符串边界的语言中处理时尤其如此。

## 如何操作：

这是一个 C 函数，可以将那些讨厌的引号从你的字符串中清除掉：

```c
#include <stdio.h>
#include <string.h>

void remove_quotes(char *str) {
    char *p_read = str, *p_write = str;
    while (*p_read) {
        if (*p_read != '"' && *p_read != '\'') {
            *p_write++ = *p_read;
        }
        p_read++;
    }
    *p_write = '\0';
}

int main() {
    char str[] = "He said, \"Hello, 'world'!\"";
    printf("Original: %s\n", str);
    remove_quotes(str);
    printf("Sanitized: %s\n", str);
    return 0;
}
```

示例输出：

```
Original: He said, "Hello, 'world'!"
Sanitized: He said, Hello, world!
```

## 深入探讨

从字符串中移除引号是自编程诞生之初就存在的任务，其中数据清洁一直是避免错误（如 SQL 注入攻击）的关键，或确保字符串可以安全地传递给可能会将引号误解为控制字符的系统。

从历史上看，不同语言以不同方式处理这个任务——有些语言内置了函数（如 Python 中的 `strip`），而其他语言，如 C，由于其注重提供给开发者更低级别的控制，需要手工实现。

替代方案包括使用库函数如 `strpbrk` 来查找引号，或者使用正则表达式（借助如 PCRE 这样的库）来处理更复杂的模式，尽管对于仅仅移除引号而言，这可能有些过于复杂。

上述实现简单地扫描字符串中的每个字符，仅将非引号字符复制到写入指针的位置。这种方式是高效的，因为它是就地完成的，不需要额外的内存来存储结果字符串。

## 另请参阅

- [C 标准库函数](http://www.cplusplus.com/reference/clibrary/)
- [PCRE - 与 Perl 兼容的正则表达式](https://www.pcre.org/)
- [理解 C 语言中的指针](https://www.learn-c.org/en/Pointers)

---
title:                "删除字符串中的引号"
date:                  2024-02-03T18:07:05.393162-07:00
model:                 gpt-4-0125-preview
simple_title:         "删除字符串中的引号"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/removing-quotes-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何为与为何？

在 C 语言中从字符串中移除引号涉及到抽取文本内容，不包括单引号（' '）或双引号（" "）。这个过程对于清理输入数据、解析文件内容或为进一步处理准备字符串（在这些情况下引号不需要或可能会导致数据处理错误）是必需的。

## 如何操作：

要在 C 语言中从字符串中移除引号，我们遍历字符串，将不是引号的字符复制到一个新的字符串中。这个过程可以根据需要移除仅头尾的引号或字符串中存在的所有引号。下面是一个示例，演示了这两种方法：

```c
#include <stdio.h>
#include <string.h>

// 函数以从字符串中移除所有引号
void removeAllQuotes(char *source, char *dest) {
    while (*source) {
        if (*source != '"' && *source != '\'') {
            *dest++ = *source;
        }
        source++;
    }
    *dest = '\0'; // 以空字符结束目标字符串
}

// 函数以只从字符串中移除头尾的引号
void removeEdgeQuotes(char *source, char *dest) {
    size_t len = strlen(source);
    if (source[0] == '"' || source[0] == '\'') source++, len--;
    if (source[len-1] == '"' || source[len-1] == '\'') len--;
    strncpy(dest, source, len);
    dest[len] = '\0'; // 以空字符结束目标字符串
}

int main() {
    char str1[] = "'Hello, World!'";
    char str2[] = "\"Programming in C\"";
    char noQuotes1[50];
    char noQuotes2[50];
    
    removeAllQuotes(str1, noQuotes1);
    printf("移除所有引号: %s\n", noQuotes1);
    
    removeEdgeQuotes(str2, noQuotes2);
    printf("移除头尾引号: %s\n", noQuotes2);
    
    return 0;
}
```
样本输出：
```
移除所有引号: Hello, World!
移除头尾引号: Programming in C
```

这些示例展示了如何处理字符串中存在的所有引号的移除以及仅定向移除头尾引号。

## 深入探讨

在 C 语言中从字符串移除引号的概念，并没有显著的历史深度，除了其与早期文本处理需求的关联之外。这里展示的直接方法是多功能的，但对于非常大的字符串或高性能要求，效率不高，在这些情况下，可能会更倾向于就地修改或更高级的算法。

使用 `strpbrk` 找到引号并移动字符串中非引号部分的替代方法可能更高效，但需要对 C 语言中的指针和内存管理有更深入的理解。此外，正则表达式库的出现为字符串操作提供了强大的工具集，包括移除引号。然而，这些库虽然强大，但增加了可能对于简单任务不必要的复杂性和开销。因此，如展示的直接方法，对于 C 程序员而言，依然是一项宝贵的技能，将简单性与许多常见用例的有效性结合起来。

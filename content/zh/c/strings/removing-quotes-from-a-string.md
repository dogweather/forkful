---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:07:05.393162-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8981\u5728 C \u8BED\u8A00\u4E2D\u4ECE\
  \u5B57\u7B26\u4E32\u4E2D\u79FB\u9664\u5F15\u53F7\uFF0C\u6211\u4EEC\u904D\u5386\u5B57\
  \u7B26\u4E32\uFF0C\u5C06\u4E0D\u662F\u5F15\u53F7\u7684\u5B57\u7B26\u590D\u5236\u5230\
  \u4E00\u4E2A\u65B0\u7684\u5B57\u7B26\u4E32\u4E2D\u3002\u8FD9\u4E2A\u8FC7\u7A0B\u53EF\
  \u4EE5\u6839\u636E\u9700\u8981\u79FB\u9664\u4EC5\u5934\u5C3E\u7684\u5F15\u53F7\u6216\
  \u5B57\u7B26\u4E32\u4E2D\u5B58\u5728\u7684\u6240\u6709\u5F15\u53F7\u3002\u4E0B\u9762\
  \u662F\u4E00\u4E2A\u793A\u4F8B\uFF0C\u6F14\u793A\u4E86\u8FD9\u4E24\u79CD\u65B9\u6CD5\
  \uFF1A."
lastmod: '2024-04-05T22:38:47.440970-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8981\u5728 C \u8BED\u8A00\u4E2D\u4ECE\u5B57\
  \u7B26\u4E32\u4E2D\u79FB\u9664\u5F15\u53F7\uFF0C\u6211\u4EEC\u904D\u5386\u5B57\u7B26\
  \u4E32\uFF0C\u5C06\u4E0D\u662F\u5F15\u53F7\u7684\u5B57\u7B26\u590D\u5236\u5230\u4E00\
  \u4E2A\u65B0\u7684\u5B57\u7B26\u4E32\u4E2D\u3002\u8FD9\u4E2A\u8FC7\u7A0B\u53EF\u4EE5\
  \u6839\u636E\u9700\u8981\u79FB\u9664\u4EC5\u5934\u5C3E\u7684\u5F15\u53F7\u6216\u5B57\
  \u7B26\u4E32\u4E2D\u5B58\u5728\u7684\u6240\u6709\u5F15\u53F7\u3002\u4E0B\u9762\u662F\
  \u4E00\u4E2A\u793A\u4F8B\uFF0C\u6F14\u793A\u4E86\u8FD9\u4E24\u79CD\u65B9\u6CD5\uFF1A\
  ."
title: "\u5220\u9664\u5B57\u7B26\u4E32\u4E2D\u7684\u5F15\u53F7"
weight: 9
---

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

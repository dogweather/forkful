---
aliases:
- /zh/c/searching-and-replacing-text/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:08:12.685082-07:00
description: "\u5728 C \u4E2D\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C\u6D89\u53CA\
  \u5230\u627E\u51FA\u4E00\u4E2A\u8F83\u5927\u5B57\u7B26\u4E32\u5185\u7684\u7279\u5B9A\
  \u5B50\u5B57\u7B26\u4E32\uFF0C\u5E76\u5C06\u5B83\u4EEC\u66FF\u6362\u6210\u4E0D\u540C\
  \u7684\u5B50\u5B57\u7B26\u4E32\u3002\u7A0B\u5E8F\u5458\u6267\u884C\u8FD9\u4E9B\u64CD\
  \u4F5C\u662F\u4E3A\u4E86\u5904\u7406\u6587\u672C\u6570\u636E - \u4EFB\u52A1\u8303\
  \u56F4\u4ECE\u6570\u636E\u6E05\u6D17\u548C\u683C\u5F0F\u5316\u5230\u52A8\u6001\u751F\
  \u6210\u5185\u5BB9\u3002"
lastmod: 2024-02-18 23:08:59.544489
model: gpt-4-0125-preview
summary: "\u5728 C \u4E2D\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C\u6D89\u53CA\u5230\
  \u627E\u51FA\u4E00\u4E2A\u8F83\u5927\u5B57\u7B26\u4E32\u5185\u7684\u7279\u5B9A\u5B50\
  \u5B57\u7B26\u4E32\uFF0C\u5E76\u5C06\u5B83\u4EEC\u66FF\u6362\u6210\u4E0D\u540C\u7684\
  \u5B50\u5B57\u7B26\u4E32\u3002\u7A0B\u5E8F\u5458\u6267\u884C\u8FD9\u4E9B\u64CD\u4F5C\
  \u662F\u4E3A\u4E86\u5904\u7406\u6587\u672C\u6570\u636E - \u4EFB\u52A1\u8303\u56F4\
  \u4ECE\u6570\u636E\u6E05\u6D17\u548C\u683C\u5F0F\u5316\u5230\u52A8\u6001\u751F\u6210\
  \u5185\u5BB9\u3002"
title: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C"
---

{{< edit_this_page >}}

## 什么 & 为什么?

在 C 中搜索和替换文本涉及到找出一个较大字符串内的特定子字符串，并将它们替换成不同的子字符串。程序员执行这些操作是为了处理文本数据 - 任务范围从数据清洗和格式化到动态生成内容。

## 如何操作:

C 并没有内置的函数可以直接对字符串进行搜索和替换。然而，你可以通过结合 `<string.h>` 库中可用的各种字符串处理函数以及一些自定义逻辑来实现这一点。下面是一个如何在字符串内搜索子字符串并替换它的基本示例。为了简化，这个例子假设有足够的缓冲区大小，并没有处理生产代码中应该考虑的内存分配问题。

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void replaceSubstring(char *source, char *sub, char *new_sub) {
    char buffer[1024];
    char *insert_point = &buffer[0];
    const char *tmp = source;
    size_t len_sub = strlen(sub), len_new_sub = strlen(new_sub);
    size_t len_up_to_match;

    while ((tmp = strstr(tmp, sub))) {
        // 计算匹配之前的长度
        len_up_to_match = tmp - source;
        
        // 复制匹配之前的部分
        memcpy(insert_point, source, len_up_to_match);
        insert_point += len_up_to_match;
        
        // 复制新的子字符串
        memcpy(insert_point, new_sub, len_new_sub);
        insert_point += len_new_sub;
        
        // 在源字符串中跳过匹配部分
        tmp += len_sub;
        source = tmp;
    }
    
    // 复制源字符串的任何剩余部分
    strcpy(insert_point, source);
    
    // 打印修改后的字符串
    printf("修改后的字符串: %s\n", buffer);
}

int main() {
    char sourceStr[] = "Hello, this is a test. This test is simple.";
    char sub[] = "test";
    char newSub[] = "sample";
    
    replaceSubstring(sourceStr, sub, newSub);
    
    return 0;
}
```

示例输出:
```
修改后的字符串: Hello, this is a sample. This sample is simple.
```

此代码展示了一个简单的方法，用于搜索源字符串中所有子字符串（`sub`）的实例，并使用 `strstr` 函数找到每个匹配的起点，将它们替换为另一个子字符串（`newSub`）。这是一个非常基础的例子，它没有处理像重叠子字符串这样的复杂情况。

## 深入探讨

"如何操作"部分中使用的方法是基础的，说明了如何在不借助任何第三方库的情况下，在 C 中实现文本搜索和替换。从历史上看，由于 C 强调底层内存管理和性能，其标准库没有封装像 Python 或 JavaScript 这样的语言中找到的高级字符串操作功能。程序员必须手动管理内存并结合使用各种字符串操作来实现期望的结果，这增加了复杂性，但提供了更多的控制和效率。

需要注意的是，这种手动方法可能容易出错，特别是在管理内存分配和缓冲区大小时。错误的处理可能导致缓冲区溢出和内存损坏，使代码易受安全风险的侵害。

在许多实际情况中，特别是在需要复杂文本处理的场景中，考虑集成第三方库如 PCRE（Perl 兼容正则表达式）进行基于正则表达式的搜索和替换往往是值得的，这可以简化代码并减少出错的可能性。此外，现代 C 标准和编译器日益提供内置函数和字符串操作的更安全替代品，旨在减轻在旧 C 代码库中观察到的常见问题。然而，对于优化性能关键应用程序而言，手动处理文本的基本理解仍然是程序员工具箱中的宝贵技能。

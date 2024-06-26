---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:08:12.685082-07:00
description: "\u5982\u4F55\u64CD\u4F5C: C \u5E76\u6CA1\u6709\u5185\u7F6E\u7684\u51FD\
  \u6570\u53EF\u4EE5\u76F4\u63A5\u5BF9\u5B57\u7B26\u4E32\u8FDB\u884C\u641C\u7D22\u548C\
  \u66FF\u6362\u3002\u7136\u800C\uFF0C\u4F60\u53EF\u4EE5\u901A\u8FC7\u7ED3\u5408 `<string.h>`\u2026"
lastmod: '2024-04-05T21:53:48.573614-06:00'
model: gpt-4-0125-preview
summary: "C \u5E76\u6CA1\u6709\u5185\u7F6E\u7684\u51FD\u6570\u53EF\u4EE5\u76F4\u63A5\
  \u5BF9\u5B57\u7B26\u4E32\u8FDB\u884C\u641C\u7D22\u548C\u66FF\u6362\u3002\u7136\u800C\
  \uFF0C\u4F60\u53EF\u4EE5\u901A\u8FC7\u7ED3\u5408 `<string.h>` \u5E93\u4E2D\u53EF\
  \u7528\u7684\u5404\u79CD\u5B57\u7B26\u4E32\u5904\u7406\u51FD\u6570\u4EE5\u53CA\u4E00\
  \u4E9B\u81EA\u5B9A\u4E49\u903B\u8F91\u6765\u5B9E\u73B0\u8FD9\u4E00\u70B9\u3002\u4E0B\
  \u9762\u662F\u4E00\u4E2A\u5982\u4F55\u5728\u5B57\u7B26\u4E32\u5185\u641C\u7D22\u5B50\
  \u5B57\u7B26\u4E32\u5E76\u66FF\u6362\u5B83\u7684\u57FA\u672C\u793A\u4F8B\u3002\u4E3A\
  \u4E86\u7B80\u5316\uFF0C\u8FD9\u4E2A\u4F8B\u5B50\u5047\u8BBE\u6709\u8DB3\u591F\u7684\
  \u7F13\u51B2\u533A\u5927\u5C0F\uFF0C\u5E76\u6CA1\u6709\u5904\u7406\u751F\u4EA7\u4EE3\
  \u7801\u4E2D\u5E94\u8BE5\u8003\u8651\u7684\u5185\u5B58\u5206\u914D\u95EE\u9898\u3002"
title: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C"
weight: 10
---

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

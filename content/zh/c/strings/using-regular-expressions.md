---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:58.765550-07:00
description: "\u6B63\u5219\u8868\u8FBE\u5F0F\uFF08regex\uFF09\u63D0\u4F9B\u4E86\u4E00\
  \u79CD\u4F7F\u7528\u5B9A\u4E49\u7684\u6A21\u5F0F\u641C\u7D22\u3001\u5339\u914D\u548C\
  \u64CD\u4F5C\u5B57\u7B26\u4E32\u7684\u65B9\u6CD5\u3002\u7F16\u7A0B\u4EBA\u5458\u5E7F\
  \u6CDB\u5229\u7528\u5B83\u4EEC\u5B8C\u6210\u5982\u9A8C\u8BC1\u8F93\u5165\u3001\u89E3\
  \u6790\u6587\u672C\u6570\u636E\u548C\u67E5\u627E\u5927\u578B\u6587\u672C\u6587\u4EF6\
  \u4E2D\u7684\u6A21\u5F0F\u7B49\u4EFB\u52A1\uFF0C\u4F7F\u5176\u6210\u4E3A\u4EFB\u4F55\
  \u8BED\u8A00\uFF08\u5305\u62ECC\u8BED\u8A00\uFF09\u4E2D\u7684\u5F3A\u5927\u5DE5\u5177\
  \u3002"
lastmod: '2024-03-13T22:44:48.304299-06:00'
model: gpt-4-0125-preview
summary: "\u6B63\u5219\u8868\u8FBE\u5F0F\uFF08regex\uFF09\u63D0\u4F9B\u4E86\u4E00\u79CD\
  \u4F7F\u7528\u5B9A\u4E49\u7684\u6A21\u5F0F\u641C\u7D22\u3001\u5339\u914D\u548C\u64CD\
  \u4F5C\u5B57\u7B26\u4E32\u7684\u65B9\u6CD5\u3002\u7F16\u7A0B\u4EBA\u5458\u5E7F\u6CDB\
  \u5229\u7528\u5B83\u4EEC\u5B8C\u6210\u5982\u9A8C\u8BC1\u8F93\u5165\u3001\u89E3\u6790\
  \u6587\u672C\u6570\u636E\u548C\u67E5\u627E\u5927\u578B\u6587\u672C\u6587\u4EF6\u4E2D\
  \u7684\u6A21\u5F0F\u7B49\u4EFB\u52A1\uFF0C\u4F7F\u5176\u6210\u4E3A\u4EFB\u4F55\u8BED\
  \u8A00\uFF08\u5305\u62ECC\u8BED\u8A00\uFF09\u4E2D\u7684\u5F3A\u5927\u5DE5\u5177\u3002"
title: "\u4F7F\u7528\u6B63\u5219\u8868\u8FBE\u5F0F"
---

{{< edit_this_page >}}

## 是什么 & 为什么？

正则表达式（regex）提供了一种使用定义的模式搜索、匹配和操作字符串的方法。编程人员广泛利用它们完成如验证输入、解析文本数据和查找大型文本文件中的模式等任务，使其成为任何语言（包括C语言）中的强大工具。

## 如何使用：

在C语言中使用正则表达式，你主要会使用POSIX正则表达式库(`<regex.h>`)。这个示例演示了基本的模式匹配：

```c
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>

int main(){
    regex_t regex;
    int return_value;
    char *pattern = "^a[[:alnum:]]"; // 匹配以'a'开头后跟字母数字字符的字符串的模式
    char *test_string = "apple123";

    // 编译正则表达式
    return_value = regcomp(&regex, pattern, REG_EXTENDED);
    if (return_value) {
        printf("无法编译正则表达式\n");
        exit(1);
    }

    // 执行正则表达式
    return_value = regexec(&regex, test_string, 0, NULL, 0);
    if (!return_value) {
        printf("找到匹配\n");
    } else if (return_value == REG_NOMATCH) {
        printf("未找到匹配\n");
    } else {
        printf("正则表达式匹配失败\n");
        exit(1);
    }

    // 释放正则表达式使用的已分配内存
    regfree(&regex);

    return 0;
}
```

对于匹配的字符串（"apple123"）的示例输出：
```
找到匹配
```
对于不匹配的字符串（"banana"）：
```
未找到匹配
```

## 深入探讨：

作为POSIX标准的一部分，在C语言中的正则表达式提供了一种健壮的执行字符串匹配和操作的方式。然而，与设计有一流字符串操作特性的语言（如Python或Perl）相比，C语言中POSIX正则表达式库的API被认为更加繁琐。模式的语法在各个语言之间是相似的，但C语言需要手动管理内存并编写更多样板代码来准备、执行和清理使用正则表达式模式后的操作。

尽管存在这些挑战，学习在C语言中使用正则表达式是有益的，因为它加深了对底层编程概念的理解。此外，它为C语言编程在文本处理和数据提取等正则表达式不可或缺的领域开辟了可能性。对于更复杂的模式或正则表达式操作，如PCRE（Perl兼容正则表达式）库可能提供了一个更加功能丰富且在某种程度上更容易的接口，尽管它需要将一个外部库集成到你的C项目中。

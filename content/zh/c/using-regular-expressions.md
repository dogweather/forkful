---
title:                "使用正则表达式"
date:                  2024-01-19
html_title:           "C: 使用正则表达式"
simple_title:         "使用正则表达式"

category:             "C"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (什么以及为什么？)
正则表达式是强大的文本模式匹配工具。程序员用它来快速搜寻、替换或校验字符串，节约时间，减少代码。

## How to: (如何做：)
```C
#include <stdio.h>
#include <regex.h>

int main() {
    regex_t regex;
    int ret;

    // 编译正则表达式
    ret = regcomp(&regex, "^a[[:alnum:]]", 0);
    if (ret) {
        printf("无法编译正则表达式\n");
        return 1;
    }

    // 执行匹配
    ret = regexec(&regex, "abc", 0, NULL, 0);
    if (!ret) {
        printf("匹配成功\n");
    } else if (ret == REG_NOMATCH) {
        printf("匹配失败\n");
    } else {
        printf("正则表达式匹配时出错\n");
    }

    // 释放正则表达式
    regfree(&regex);
    return 0;
}
```
输出:
```
匹配成功
```

## Deep Dive (深入了解)
正则表达式起源于1950年代的理论计算机科学。C中正则通过`<regex.h>`库实现，但它比现代语言如Python中的正则表达实现简单。有时候，`grep`或文本编辑器内置功能能做同样的事。

## See Also (另请参阅)
- POSIX正则表达式维基: https://en.wikipedia.org/wiki/Regular_expression#POSIX_basic_and_extended
- C库`<regex.h>`手册: https://man7.org/linux/man-pages/man3/regex.3.html
- 线上正则表达式测试器: https://regex101.com/

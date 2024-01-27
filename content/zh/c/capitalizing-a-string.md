---
title:                "字符串首字母大写"
date:                  2024-01-19
html_title:           "Arduino: 字符串首字母大写"
simple_title:         "字符串首字母大写"
programming_language: "C"
category:             "C"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (什么和为什么？)

大写字符串就是将所有字符转换成大写字母。程序员这么做通常为了统一格式或者提高可读性。

## How to: (怎么做：)

在C语言中，你可以使用`<ctype.h>`库中的`toupper`函数来实现：

```c
#include <stdio.h>
#include <ctype.h>

void capitalizeString(char *str) {
    while(*str) {
        *str = toupper((unsigned char) *str);
        str++;
    }
}

int main() {
    char text[] = "hello world";
    capitalizeString(text);
    printf("Capitalized String: %s\n", text);
    return 0;
}

// 输出: Capitalized String: HELLO WORLD
```

## Deep Dive (深入研究)

在历史上，大写字母是书写时用于开始句子和名称的规则。在计算机编程中，大写转换通常用于用户输入规范化，比如搜索引擎忽略大小写进行搜索匹配。

在C中除了`<ctype.h>`和`toupper`，你还可以直接通过ASCII值来做转换。这种方式需要手动检查字符范围。大写字母的ASCII值从65到90，小写字母的ASCII值从97到122。

关于实现，你需要小心地处理多个字符集和本地化问题。比如，某些欧洲语言有特殊字符无法简单地通过ASCII转换来大写化。

## See Also (另请参见)

- C标准库参考: http://www.cplusplus.com/reference/cctype/
- ASCII码表: https://www.asciitable.com/
- 字符编码和国际化支持: https://www.unicode.org/

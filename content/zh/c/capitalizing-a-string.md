---
title:    "C: 字符串大写化"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 为什么

在编程中，经常会遇到需要将字符串中的字母转换成大写的情况。这通常是为了统一格式或者满足某些特定的需求。在C语言中，有几种方法可以实现字符串的大写转换。下面将介绍如何使用C语言来实现字符串的大写转换，以及更深入的信息。

## 如何

在C语言中，可以使用内置函数`toupper()`来将字符串中的每个字母转换为大写。示例如下：

```C
#include <stdio.h>
#include <string.h>
#include <ctype.h>

int main()
{
    char str[50];

    printf("请输入一个字符串：");
    scanf("%s", str);

    // 使用toupper函数将字符串中的每个字母转换为大写
    for (int i = 0; i < strlen(str); i++)
    {
        str[i] = toupper(str[i]);
    }

    printf("转换后的字符串为：%s", str);
    return 0;
}

```

输入：hello world
输出：HELLO WORLD

如果想要将字符串中的某个字母转换为大写，可以使用`toupper()`函数中的第二个参数来指定要转换的字母的位置，示例如下：

```C
#include <stdio.h>
#include <string.h>
#include <ctype.h>

int main()
{
    char str[] = "hello world";

    // 将字符串中的第一个字母转换为大写
    str[0] = toupper(str[0]);

    printf("转换后的字符串为：%s", str);
    return 0;
}

```

输出：Hello world

## 深入了解

除了使用`toupper()`函数之外，还可以使用C语言的字符操作函数来实现字符串的大写转换。例如，可以使用`islower()`函数来判断一个字符是否为小写字母，如果是，则可以使用`toupper()`函数将其转换为大写。同时，也可以使用`tolower()`函数来将一个字符转换为小写字母。更多关于字符操作函数的信息，可以参考C语言文档。

## 参考资料

- [C语言文档](https://www.runoob.com/cprogramming/c-standard-library-string-h.html)
- [toupper函数](https://www.runoob.com/cprogramming/c-function-toupper.html)
- [islower函数](https://www.runoob.com/cprogramming/c-function-islower.html)
- [tolower函数](https://www.runoob.com/cprogramming/c-function-tolower.html)

## 参见

- [C语言字符串操作](https://www.runoob.com/cprogramming/c-strings.html)
- [Markdown语法指南](https://www.runoob.com/markdown/md-tutorial.html)
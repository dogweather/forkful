---
title:                "将字符串大写化"
html_title:           "C: 将字符串大写化"
simple_title:         "将字符串大写化"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 第一部分: 什么和为何？

将字符串大写是指将字符串中的所有字符都转换为大写形式。开发者会这样做是因为在某些情况下，忽视大小写可以简化字符串的比较和搜索。

# 第二部分：怎么做？

```C
#include <ctype.h>
#include <stdio.h>

//函数：使字符串全部大写
void capitalize(char* str)
{
    for(int i = 0; str[i] != '\0'; i++){
        str[i] = toupper(str[i]);
    }
}

int main()
{
    char str[] = "hello, world!";
    capitalize(str);
    printf("%s", str);  // 输出 "HELLO, WORLD!"
    return 0;
}
```

以上代码定义了一个名为`capitalize`的函数，用于将输入的字符串`str`转换为全大写形式。使用`ctype.h`库的`toupper`函数实现每个字符的转换。

# 第三部分：深度剖析

字符串大写化在早期的计算机科学中已广为使用，尤其在用户输入和文本数据处理之中。尽管这个操作在过去的编程语言中并不直观，但是在C语言等现代编程语言中已经变得很直观。

大写化字符串的另一种方式是通过ASCII值的操作。我们知道，小写字母的ASCII值是其对应的大写字母的ASCII值加32。因此，对字符串中的每个字符，我们可以减去32，这样就可以得到对应的大写字母。

另外，`toupper`函数在内部处理了大写转换。它接受一个`int`类型的参数。如果参数在ASCII中对应的字符为小写字母，那么它返回的结果将是对应的大写字母的ASCII值，否则返回原值。

# 第四部分：参考资源

- C库函数 - toupper() - http://www.cplusplus.com/reference/cctype/toupper
- C Programming/Strings - https://en.wikibooks.org/wiki/C_Programming/Strings
- C语言字符串处理 - https://www.runoob.com/cprogramming/c-string.html
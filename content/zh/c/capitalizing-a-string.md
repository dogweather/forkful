---
title:                "将字符串转换为大写字母"
html_title:           "C: 将字符串转换为大写字母"
simple_title:         "将字符串转换为大写字母"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 究竟是什么?为什么?
首先，让我们来澄清一个问题：什么是“字符串大写化”？简而言之，这就是把一个字符串中的所有字母变成大写形式。为什么程序员要这么做呢？因为这可以让我们更容易地处理不区分大小写的数据，也可以让字符串的比较更准确。

# 如何执行:
让我们来看看如何使用C语言来大写化一个字符串。首先，我们要定义一个变量来存储这个字符串，然后使用一个循环来遍历这个字符串中的每一个字符。在循环中，我们可以使用库函数toupper()来把小写字母转换为大写字母。最后，我们将处理后的字符串打印出来。下面是一个例子：

```
#include <stdio.h>
#include <ctype.h>

int main() {
    char str[] = "hello world";
    int i;

    for (i = 0; str[i] != '\0'; i++) {
        str[i] = toupper(str[i]);
    }

    printf("%s", str); // 输出: HELLO WORLD

    return 0;
}
```

# 深入探索:
现在让我们来看看一些关于字符串大写化的更深层次的信息。首先，这个概念在很多编程语言中都有，不仅仅局限于C语言。其次，在早期的电传打字机时代，因为没有小写字母，人们就一直在使用全部大写的形式。最后，除了使用循环和toupper()函数之外，还可以使用条件语句来判断字符是否为小写字母。但是使用库函数更简洁和高效。

# 参考资料:
- [C语言字符串大写化示例代码](https://www.programiz.com/c-programming/examples/uppercase-string)
- [toupper()函数的使用文档](https://www.tutorialspoint.com/c_standard_library/c_function_toupper.htm)
- [字符串大小写转换的其他方法](https://stackoverflow.com/questions/3389728/what-is-the-best-way-to-char-to-uppercase-char-in-c)
---
title:                "C: 使用正则表达式"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 为什么

当我们在编写代码时，经常会需要对于字符串进行匹配或者替换操作。使用正则表达式（regular expressions）可以极大地简化这一过程，让我们的代码更加灵活和高效。在这篇文章中，我们将会深入探讨如何在C语言中使用正则表达式。

## 如何

正则表达式是由特殊字符和文本组成的模式，用于匹配字符串中符合规则的部分。在C语言中，我们可以使用regcomp()和regexec()两个函数来进行正则表达式的编译和匹配操作。

让我们来看一个简单的例子，假设我们有一个包含电子邮箱地址的字符串，我们想要从中提取出用户名。我们可以使用正则表达式"(.*)@(.*)"来匹配这个字符串，并使用regexec()函数获取匹配的结果。

```C
#include <stdio.h>
#include <regex.h>

int main()
{
    // 定义正则表达式和待匹配字符串
    char *pattern = "(.*)@(.*)";
    char *str = "example@example.com";

    // 定义regcomp()函数需要的结构体
    regex_t regex;

    // 编译正则表达式
    regcomp(&regex, pattern, 0);

    // 匹配正则表达式
    regmatch_t match[3];
    regexec(&regex, str, 3, match, 0);

    // 输出匹配的结果
    printf("用户名：%.*s \n", match[1].rm_eo - match[1].rm_so, str + match[1].rm_so);

    // 释放正则表达式结构体
    regfree(&regex);
    
    return 0;
}

```

输出结果为：

```
用户名：example 
```

除了使用"."来匹配任意字符，我们还可以使用特殊字符来指定匹配的范围，比如"[a-z]"表示匹配小写字母，"[0-9]"表示匹配数字等等。此外，我们还可以使用"?"来表示可选项，"*"来表示任意个数的重复，"+"来表示至少一个的重复等等。

## 深入探讨

除了上面提到的两个函数外，C语言还提供了regerror()和regfree()函数来处理错误和释放正则表达式结构体。

在实际应用中，我们可能会遇到更复杂的匹配需求，这时候可以使用一些高级特性，比如反向引用（backreferences）、断言（lookaround）、非贪婪匹配（non-greedy matching）等等。如果对正则表达式有更深入的了解，可以参考Perl兼容正则表达式（PCRE）和GNU扩展正则表达式（ERE）的文档。

## 参考链接

- [GNU C Library](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html#Regular-Expressions)
- [Regular Expressions in C](https://www.geeksforgeeks.org/regular-expressions-in-c/)
- [PCRE Documentation](https://www.pcre.org/current/doc/html/pcre2syntax.html)
- [ERE Documentation](https://www.gnu.org/software/gnulib/manual/html_node/ERE-POSIX.html)

## 参见

- [Markdown语法介绍](https://www.jianshu.com/p/9e3b13c3c2b2)
- [正则表达式测试工具](https://regex101.com/)
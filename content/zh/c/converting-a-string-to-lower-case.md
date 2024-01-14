---
title:    "C: 将字符串转换为小写"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

为什么要将字符串转换为小写？

有时候在编程中，我们需要将字符串转换为小写形式。这通常是为了方便比较字符串，或者是为了避免大小写造成的错误。使用C语言来进行这样的转换是非常简单的。让我们一起来看看如何实现它。

如何进行字符串大小写转换：

```C
#include <stdio.h>
#include <string.h>

int main()
{
    // 声明一个字符串变量
    char str[] = "Hello world";
    
    // 使用库函数strlwr()将字符串转换为小写形式
    strlwr(str);
    
    // 输出转换后的字符串
    printf("%s\n", str);
    
    return 0;
}

```

输出结果为：

```C
hello world
```

深入探讨：

转换字符串为小写形式的过程实际上是将字符串中的每个字母变成其小写形式。在C语言中，我们可以使用库函数strlwr()来实现这一功能。这个函数的原理是将字符串中的每个字符与ASCII码中的小写字母表进行比较，如果字符的ASCII码在大写字母的范围内，则将其转换为小写字母，否则不做改变。

使用这个函数的好处是可以节省我们自己写转换代码的时间和精力。而且，它还能有效避免我们可能会犯的错误，比如忘记转换某个字符或者转换错误的情况。

还值得一提的是，在一些编译器中，可能没有提供strlwr()函数，这时我们可以使用一个循环来遍历字符串，将每个字符转换为小写形式。

```C
#include <stdio.h>

int main()
{
    // 声明一个字符串变量
    char str[] = "Hello world";
    
    // 使用循环来遍历字符串并转换为小写形式
    for(int i = 0; str[i] != '\0'; i++)
    {
        // 如果当前字符是大写字母，则转换为小写字母
        if(str[i] >= 'A' && str[i] <= 'Z')
        {
            str[i] = str[i] + 32;
        }
    }
    
    // 输出转换后的字符串
    printf("%s\n", str);
    
    return 0;
}
```

输出结果为：

```C
hello world
```

总的来说，无论是使用库函数还是自己写转换代码，都是很简单的。只要我们知道了这个转换过程，就可以根据自己的需求来选择合适的方法。

另外，如果我们需要将字符串转换为大写形式，也可以使用库函数strupr()来实现，其原理与strlwr()相反。

## 参考链接：

- [C语言字符串小写字母转换函数strlwr()的用法及原理](https://www.jianshu.com/p/97d7104c3acd)
- [C语言字符串大小写转换的一种简单实现方法](https://blog.csdn.net/qq_31658485/article/details/80176035)
- [ASCII码表](https://www.asciitable.com/)
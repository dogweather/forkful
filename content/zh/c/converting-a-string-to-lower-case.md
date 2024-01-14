---
title:    "C: 将字符串转换为小写"
keywords: ["C"]
---

{{< edit_this_page >}}

## 为什么要做字符串转换为小写

在编程过程中，字符串(string)是一个非常常见的数据类型。有时候，我们需要将字符串中的所有字母都转换为小写形式。这样做的主要原因可能是为了统一格式，或者是方便后续的比较、查找等操作。下面我们来看看如何实现这一功能。

## 如何做字符串转换为小写

在C语言中，我们可以使用标准库中的`tolower()`函数来实现字符串转换为小写。该函数的原型为`int tolower(int c)`，它接受一个字符作为参数，并返回转换后的小写形式字符。下面是一个简单的代码示例，展示如何对字符串进行转换并输出结果。

```C
#include <stdio.h>
#include <ctype.h>

int main()
{
    char str[] = "Hello World!";
    int i;

    printf("Original string: %s\n", str);

    for (i = 0; str[i] != '\0'; i++)
    {
        str[i] = tolower(str[i]);
    }

    printf("String after conversion: %s\n", str);

    return 0;
}
```

运行结果：

```
Original string: Hello World!
String after conversion: hello world!
```

这里我们首先定义一个字符串`str`，并初始化为`"Hello World!"`。然后使用`for`循环来遍历字符串中的每个字符，并通过`tolower()`函数将其转换为小写形式。最后将转换后的字符串打印出来。

## 深入了解

在上面的例子中，我们使用了标准库中的`tolower()`函数来完成字符串转换操作。但其实，在C语言中还有其他方法可以实现相同的功能。比如可以使用位运算符来实现转换，这个方法相对比较复杂，但是效率可能会更高。另外，我们在上面的例子中只处理了英文字符，但是如果遇到其他语言的字符，比如中文、日文等，就需要考虑到不同字符集的编码方式。总之，如何实现字符串转换为小写并没有唯一的方法，我们可以根据需求来选择最适合的方法。

## 参考链接

- [C语言标准库中的tolower()函数](https://www.runoob.com/cprogramming/c-function-tolower.html)
- [如何使用位运算符来转换字符串为小写](https://stackoverflow.com/questions/5976921/efficiently-converting-a-string-to-upper-lower-case-in-c)
- [处理不同字符集的方法](https://www.cs.princeton.edu/courses/archive/spr02/cs126/lectures/l17-example.html)

## 参见

- [将字符串转换为大写](https://www.mdeditor.tw/pl/p1dS)
- [C语言字符串处理函数大集合](https://blog.csdn.net/qq_43780321/article/details/107241944)
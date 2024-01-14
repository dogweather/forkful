---
title:    "C: 查找字符串长度"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 为什么

在编程过程中，我们经常需要操作字符串。而要对字符串进行操作，了解字符串的长度是非常重要的。因此，了解如何找到字符串的长度在编程中是非常有用的。

## 怎么做

常用的方法是使用C语言中的内置函数`strlen()`来找到字符串的长度。下面是一个基本的例子：

```C
#include<stdio.h>
#include<string.h>

int main(void) {
  char str[] = "Hello World";

  // 使用strlen()函数找到字符串的长度
  int len = strlen(str);

  // 输出字符串的长度
  printf("字符串的长度为: %d\n", len);

  return 0;
}
```

输出结果：

```
字符串的长度为: 11
```

除了使用`strlen()`函数，我们也可以利用循环来遍历字符串的每个字符，直到遇到字符串的末尾`\0`，来计算字符串的长度。下面是一个使用循环的例子：

```C
#include<stdio.h>

int main(void) {
  char str[] = "Hello World";

  // 声明变量用来计数字符串的长度
  int len = 0;

  // 使用循环遍历每个字符，直到遇到字符串的末尾
  while(str[len] != '\0') {
    // 计数加一
    len++;
  }

  // 输出字符串的长度
  printf("字符串的长度为: %d\n", len);

  return 0;
}
```

输出结果：

```
字符串的长度为: 11
```

## 深入探讨

在C语言中，字符串实际上是一个字符数组，以`\0`结尾。因此，我们可以利用数组中每个元素的下标来访问字符串中的每个字符。`strlen()`函数就是利用这个原理来计算字符串的长度，它会从字符串的第一个字符开始遍历，直到遇到`\0`为止，并返回遍历的次数作为字符串的长度。

另外，由于`strlen()`函数会遍历整个字符串，因此它的时间复杂度是`O(N)`，其中N为字符串的长度。因此，如果字符串太长，使用循环来计算字符串的长度可能更有效率。

## 参考资料

- [C语言教程-字符串的处理](https://www.runoob.com/cprogramming/c-string.html)
- [string.h——字符串中各种函数的使用方法](https://blog.csdn.net/peng19921125/article/details/50458482)
- [The C Library Reference Guide - strlen](https://www-s.acm.illinois.edu/webmonkeys/book/c_guide/2.10.html)

## 参见

- [C语言中的字符数组和字符串](https://www.jianshu.com/p/a8026f96b3c5)
- [C语言中的循环结构](https://www.runoob.com/cprogramming/c-for-loop.html)
- [C语言中的函数](https://www.runoob.com/cprogramming/c-functions.html)
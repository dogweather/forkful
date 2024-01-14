---
title:                "C: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/extracting-substrings.md"
---

{{< edit_this_page >}}

# 为什么要提取子字符串？

提取子字符串是一个常用的技术，在编程中经常会遇到这样的情况：我们需要从一个字符串中提取出特定的一部分，而不是使用整个字符串。比如，从一个完整的文件路径中获取文件名，从一个邮箱地址中提取用户名等等。这就是为什么能够熟练地提取子字符串是一个程序员必备的技能。

# 如何提取子字符串？

在C语言中，提取子字符串可以使用几个不同的方法。首先，我们需要使用字符串函数`strncpy()`来将字符串的一部分复制到一个新的字符数组中。例如，假设我们有一个字符串`str`，我们想从第5个字符开始复制4个字符到新数组`substr`中，代码如下：

```C
char str[] = "Hello World";
char substr[5];

strncpy(substr, &str[4], 4);
// 这里的`&str[4]`表示从第5个字符开始，`4`表示复制的字符数量，可以根据具体需求进行调整

printf("提取的子字符串为：%s\n", substr);
// 输出：World
```

另一种方法是使用`strtok()`函数来将字符串按照指定的分隔符进行分割，然后取出需要的子字符串。例如，我们有一个字符串`str`，其中包含了若干个单词以空格分隔，我们想要提取出第二个单词，代码如下：

```C
char str[] = "Hello World Goodbye";
char *token;

// 第一次调用strtok()时，需要传入要分割的字符串和分隔符，随后每次调用时传入NULL
token = strtok(str, " ");

// 经过一次调用后，token指向第一个单词"Hello"
// 继续传入NULL，返回的token指向第二个单词"World"
token = strtok(NULL, " ");

printf("提取的子字符串为：%s\n", token);
// 输出：World
```

还有一种方法是使用指针来直接指向字符串中的某一部分，然后使用`puts()`函数打印出来。例如，我们有一个字符串`str`，我们想要提取出第3个字符到第5个字符之间的子字符串，代码如下：

```C
char str[] = "Hello World";
char *substr;

substr = &str[2];
// substr指向第3个字符"H"

printf("提取的子字符串为：%.*s\n", 3, substr);
// 输出：llo
```

需要注意的是，以上提取子字符串的方法都不会改变原始字符串，而是将提取的子字符串复制到新的字符串中，因此原始字符串的内容没有变化。

# 深入了解提取子字符串

如果想要更加深入地了解提取子字符串的原理，可以深入研究C语言中字符串的内存表示方式。字符串实际上是一个字符数组，使用指针来指向字符串的首地址。因此，当我们提取子字符串时，实际上是在操作指针，将指针指向对应的位置。在内存中，字符串是连续存储的，因此提取子字符串时，只需要计算出起始位置和结束位置对应的指针即可。

# 参考链接

- [C语言字符串库函数](https://www.runoob.com/c-programming/c-standard-library-functions.html)
- [指针和数组](https://www.runoob.com/cprogramming/c-pointers-arrays.html)
- [C语言中的字符串与指针](https://www.jianshu.com/p/a6c7ac0865bb)

# 查看也可

- [如何处理C语言中的字符串](https://www.example.com/how-to-handle-strings-in-c)
- [掌握C语言中常用的字符串处理技巧](https://www.example.com/master-common
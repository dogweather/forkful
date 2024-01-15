---
title:                "提取子字符串"
html_title:           "C: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/extracting-substrings.md"
---

{{< edit_this_page >}}

# 为什么

人们为什么会对提取子串感兴趣呢？首先，提取子串是一种常见的字符串操作，可以帮助解决很多实际问题。其次，掌握提取子串的技巧可以让我们在写程序时更加得心应手，提高代码效率和质量。

# 如何做

我们可以使用C语言中的字符串相关函数来提取子串。下面给出几个例子。

```
// 定义一个字符串
char str[] = "Hello World!";
// 提取从索引为6的子串，长度为5
char subStr[6];
strcpy(subStr, str + 6);
printf("%s", subStr); // 输出 "World!"
```

```
// 定义一个字符串
char str[] = "Programming in C";
// 提取从索引为12的子串，直到结尾
char subStr[6];
strcpy(subStr, str + 12);
printf("%s", subStr); // 输出 "C"
```

```
// 定义一个字符串
char str[] = "This is a string";
// 提取从索引为2的子串，长度为6
char subStr[7];
strncpy(subStr, str + 2, 6);
subStr[6] = '\0';
printf("%s", subStr); // 输出 "is a s"
```

# 深入了解

在C语言中，提取子串的底层实现是通过指针来实现的。当我们使用类似`str + 6`的操作时，实际上是将指针指向原始字符串中对应索引的位置。然后利用字符串拷贝函数来将指定长度的子串复制到目标数组中。

除了上述提到的函数外，C语言还提供了其他一些函数来帮助我们提取子串，如`strnlen()`、`strncpy()`等。熟练掌握这些函数可以帮助我们更加灵活地处理字符串操作。

# 参考资料

- [C语言标准库 | 提取子串](https://www.cplusplus.com/reference/cstring/strncpy/)
- [C语言字符串 | 从字符串中提取子串](https://www.tutorialspoint.com/c_standard_library/c_function_strncpy.htm)
- [C语言指针 | 指针初探](https://wizardforcel.gitbooks.io/learn-c-the-hard-way-cn/content/chS6.html)

# 另请参阅

- [C语言教程 | 字符串操作](https://www.runoob.com/cprogramming/c-string.html)
- [C语言函数速查表 | 字符串函数](https://www.tutorialspoint.com/cprogramming/c_string_functions.htm)
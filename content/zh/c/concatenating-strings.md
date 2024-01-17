---
title:                "字符串拼接"
html_title:           "C: 字符串拼接"
simple_title:         "字符串拼接"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## 什么是字符串连接? 为什么程序员会这样做?

字符串连接是将多个字符串链接在一起来创建新的字符串的过程。程序员经常使用它来将不同的变量或文本组合在一起以创建动态的结果。这样可以使代码更加灵活和可读。

## 如何进行字符串连接:

```C 
// 使用`+`运算符来连接两个字符串
char str1[] = "Hello ";
char str2[] = "world!";
char concat_str[12];
strcpy(concat_str, str1 + str2);
printf("结果是: %s", concat_str);
// 输出: Hello world!

// 使用`strcat()`函数来连接两个字符串
char str1[] = "Hello ";
char str2[] = "world!";
char concat_str[12];
strcat(concat_str, str1);
strcat(concat_str, str2);
printf("结果是: %s", concat_str);
// 输出: Hello world!
```

## 深入了解:

字符串连接的历史可以追溯到早期的编程语言，如BASIC和FORTRAN。除了使用运算符和函数，也可以使用基于指针的方法来连接字符串。另外，有一些其他的替代方案，如使用字符串模板或字符串格式化函数来创建动态的字符串。

## 参考资料:

- [C语言字符串连接](https://www.programiz.com/c-programming/c-strings-concatenation)
- [字符串连接的历史发展](https://en.wikipedia.org/wiki/String_concatenation)
- [字符串模板的用法](https://www.geeksforgeeks.org/string-templates-python/)
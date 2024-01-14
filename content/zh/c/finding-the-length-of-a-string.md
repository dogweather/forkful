---
title:                "C: 寻找字符串的长度"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 为什么？
在C语言中，字符串是一种非常常见的数据类型。不管是存储用户输入还是处理文本数据，字符串都是不可或缺的。而计算字符串的长度是一个非常基础的计算任务，因此了解如何找到字符串的长度是非常重要的。

## 如何？
计算字符串长度的方法非常简单。首先，我们需要定义一个字符串变量，并用合适的值对其进行初始化，如下所示：
```C
char str[] = "hello";
```
然后，我们可以使用内置函数`strlen()`来计算字符串的长度，并将其赋值给一个整型变量，如下所示：
```C
int length = strlen(str);
```
最后，我们可以使用`printf()`函数来输出字符串的长度，如下所示：
```C
printf("The length of the string is %d", length);
```
运行上面的代码，你将会得到以下输出：
```
The length of the string is 5
```

## 深入探讨
那么，`strlen()`函数是怎么计算字符串的长度的呢？其实，这个函数的实现并不复杂。它会从字符串的第一个字符开始，逐个向后遍历，直到遇到字符串的结尾标记`'\0'`，然后返回遍历过的字符数量。

需要注意的是，`strlen()`函数只会计算字符串中实际存储的字符数量，不会计算结尾的结尾标记`'\0'`。因此，如果你的字符串中包含了`'\0'`，那么`strlen()`函数会把它当做普通的字符来计算。这也是为什么建议在定义字符串时，确保最后一个字符是`'\0'`。

除了使用`strlen()`函数，还有其他方法来计算字符串的长度，比如使用循环来遍历字符串，并使用`count`变量来记录字符数量。但是，`strlen()`函数的实现已经经过了优化，因此效率更高。

## 查看更多
- [C语言教程：字符串](https://www.runoob.com/cprogramming/c-strings.html)
- [strcpy() vs strncpy() in C language](https://www.geeksforgeeks.org/strcpy-vs-strncpy-c/)
- [gettext: 在C语言中处理字符串](https://www.gnu.org/software/gettext/manual/html_node/Handling-Strings.html)

## 查看也许
以上我们介绍了如何使用`strlen()`函数来计算字符串的长度，但是在实际的项目中，我们可能还需要处理更复杂的字符串操作。比如，如何在C语言中将字符串转换为大写或小写，如何给字符串添加其他字符，如何从字符串中截取子串等等。如果你对这些操作感兴趣，不妨继续学习C语言的相关教程。
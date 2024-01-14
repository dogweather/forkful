---
title:                "C++: 字符串连接"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

##为什么

当我们需要将多个字符串连接在一起时，我们需要使用字符串连接。这可以帮助我们在编程过程中更有效地处理字符串，并减少代码重复。

##怎么做

字符串连接最简单的方法是使用 "+"运算符，它将两个字符串连接在一起。例如：
```C++
string hello = "Hello";
string world = "World";
string helloWorld = hello + world;
cout << helloWorld;
```
输出：HelloWorld

我们也可以使用"append"函数来连接字符串，它可以接受多个字符串作为参数。例如：
```C++
string sentence = "This is";
sentence.append(" a sentence").append(" using append function.");
cout << sentence;
```
输出：This is a sentence using append function.

如果我们想要在每个字符串之间添加一个空格，我们可以使用"join"函数。例如：
```C++
string fruits[] = {"apple", "banana", "orange"};
string joinedFruits = join(begin(fruits), end(fruits), " ");
cout << joinedFruits;
```
输出：apple banana orange

##深入了解

字符串连接的底层原理是将两个字符串的内容复制到一个新的字符串中。因此，在处理大量字符串时，使用字符串连接可能会导致性能下降。为了避免这种情况，我们可以使用"StringBuilder"类来动态构建字符串。它可以避免频繁地创建新的字符串，并提高执行效率。

另外，当我们需要连接数字和字符串时，我们需要将数字转换为字符串。可以使用"to_string"函数来实现此功能。例如：
```C++
int number = 123;
string result = "The number is " + to_string(number);
cout << result;
```
输出：The number is 123

## 参考链接

- [C++字符串的连接方法](https://www.cnblogs.com/wangjiquan/p/10190460.html)
- [C++中的字符串连接](https://blog.csdn.net/yygb227/article/details/40936893)
- [C++中StringBuilder的使用方法](https://blog.csdn.net/lucifer_xf/article/details/38470873)
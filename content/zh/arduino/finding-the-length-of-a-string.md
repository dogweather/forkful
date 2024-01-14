---
title:                "Arduino: 计算无素的长度"
simple_title:         "计算无素的长度"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 为什么：
在编程过程中，经常会遇到需要获取字符串长度的情况。例如，想要知道用户输入的信息是否符合要求，就需要先获取输入字符串的长度进行验证。因此，学习如何获取字符串的长度是非常有用的。

## 如何做：
在Arduino中，我们可以使用内置函数`strlen()`来获取字符串的长度。下面是一个简单的例子，展示如何使用这个函数：
```Arduino
char message[] = "Hello World"; //定义一个字符串变量
int length = strlen(message); //调用strlen()函数获取字符串长度
Serial.println(length); //打印出字符串的长度
```
这段代码的输出将会是`11`，对应字符串`"Hello World"`的长度。另外，我们也可以直接在函数中使用字符串字面量来获取长度，而不需要先将其赋值给一个变量：
```Arduino
int length = strlen("Hello World"); //直接获取字符串字面量的长度
```

## 深入探讨：
值得注意的是，`strlen()`函数返回的是`size_t`类型的值。`size_t`是一种无符号整数类型，在不同的平台上可能会有不同的大小。因此，在进行任何计算之前，我们应该先将其转换为`int`类型。另外，`strlen()`函数只能用于ASCII编码的字符串，在处理UTF-8等其他编码方式的字符串时，可能会出现问题。

## 参考链接：
- [Arduino官方文档 - strlen()函数](https://www.arduino.cc/reference/zh/language/variables/data-types/size_t/)
- [真言的博客 - Arduino strlen() 函数详解](https://blog.csdn.net/mr__zhang/article/details/42299699)
- [芯舞者的博客 - Arduino中的字符串操作函数](https://blog.csdn.net/airunknown/article/details/88128000)

## 参见：
- [Arduino官方文档 - 字符串处理函数](https://www.arduino.cc/reference/zh/language/variables/data-types/string/functions/)
---
title:                "将字符串转化为大写"
html_title:           "Arduino: 将字符串转化为大写"
simple_title:         "将字符串转化为大写"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 什么和为什么？
大写化字符串就是把字符串中的所有小写字母改成大写字母。程序员通常这样做是为了统一格式，以便比较和分类。

## 如何做：
Arduino中，`toUpperCase()`函数可以实现大写化字符串。看下面的代码：
```Arduino
String str = "arduino";
str.toUpperCase();
Serial.println(str); // 输出：ARDUINO
```
代码会输出 "ARDUINO"，即字符串变大写。

## 深入探讨
大写化字符串的需求在计算机系统早期就存在，当时只有大写字母。后来出现了大小写字母和大小写字符串区分的情况。其中，“roBurndCd”这样的混合大小写字符串是为了便于阅读而设计的，与大写化字符串的目的正好相反。

在Arduino中，除了`toUpperCase()`函数，还可以用`toCharArray()`函数配合`toupper()`函数逐个字符地大写化字符串。

实现大写化字符串的具体方式主要取决于编程环境和开发者的选择。`toUpperCase()`函数只需要一行代码，效率高，易于理解和操作；而`toupper()`函数更灵活，可以用于更复杂的字符串操作。

## 相关资料：
更深入的字符串处理教程: [字符对字符的处理](https://www.w3schools.com/cpp/cpp_strings.asp)
Arduino编程中文版: [Arduino中文编程手册](https://www.arduino.cn/thread-3671-1-1.html)
更新的String类参考: [Arduino String函数](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)
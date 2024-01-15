---
title:                "寻找字符串的长度"
html_title:           "Arduino: 寻找字符串的长度"
simple_title:         "寻找字符串的长度"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 为什么
编程时，经常会遇到需要获取字符串的长度的情况。例如，在制作一个计数器的时候，就需要获取用户输入的字符串长度来显示计数的数字。因此，学习如何找到字符串的长度将帮助您更好地完成编程任务。

## 如何做
```Arduino
// 定义一个字符串
String myString = "Hello World!";

// 使用.length()函数获取字符串的长度
int length = myString.length();

// 将字符串的长度打印出来
Serial.println(length);
```

运行以上代码，串口监视器将输出：12，表示字符串“Hello World!”的长度为12个字符。您也可以用相同的方法获取用户输入的字符串的长度，从而在需要时对其进行操作。

## 深入探究
在Arduino中，字符串是一种特殊类型的数据，其长度无法像整数型或浮点型数据那样直接获取，而是需要使用.length()函数来获取。该函数返回的是一个整数值，表示字符串的字符数。同时，空白字符也会被计算在字符串的长度之内，因此，在计算字符串长度时，请确保考虑到这一点。

## 参考链接
- Arduino官方文档：http://www.arduino.cc/reference/en/language/variables/data-types/string/functions/label/length/
- Arduino字符串教程：https://www.arduino.cc/reference/tr/language/variables/data-types/string/length/
- Ardu App：https://www.arduinohub.cn/?q=node/724
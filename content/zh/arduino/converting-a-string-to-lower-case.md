---
title:                "将字符串转换为小写"
html_title:           "Arduino: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么

*为什么要将字符串转换为小写？* 首先，它可以让我们的代码更加规范和统一，避免出现大小写混乱的情况。其次，它也可以提高程序的性能，因为比较字符串时，转换为小写后只需要比较ASCII码，效率更高。

## 如何操作

在Arduino中，我们可以使用[`toLowerCase()`](https://www.arduino.cc/reference/zh/language/functions/communication/serial/tolowercase/)函数来将字符串转换为小写。

```Arduino
// 声明并赋值字符串
String str = "Hello Arduino";

// 转换为小写并输出
Serial.println(str.toLowerCase()); // hello arduino
```

## 深入了解

在计算机中，每个字符都对应着一个ASCII码，其中大写字母的ASCII码与小写字母的ASCII码存在固定的偏移量。因此，我们可以通过对ASCII码进行简单的运算来实现字符串转换为小写的功能。

## 参考文献

- [Arduino官方文档 - `toLowerCase()`函数](https://www.arduino.cc/reference/zh/language/functions/communication/serial/tolowercase/)
- [ASCII码表](http://ascii.911cha.com/)
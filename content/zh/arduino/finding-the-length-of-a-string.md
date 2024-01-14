---
title:    "Arduino: 计算字符串的长度"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 为什么会有Arduino编程

Arduino编程是一种简单又灵活的方式来控制电子设备，它使得物联网和智能家居等概念成为可能。它能够帮助我们构建各种各样的项目，从简单的LED闪光到复杂的机器人控制，甚至是可穿戴设备。而了解如何找出字符串的长度，则是Arduino编程中非常重要的一部分。

## 如何找出字符串的长度

在Arduino编程中，字符串是由一系列的字符组成的。找出字符串的长度，可以帮助我们更好地管理和处理字符串，并且可以在循环中使用这个长度来控制代码的执行次数。

```Arduino
// 定义一个字符串变量
String str = "Hello World";

// 使用length()函数找出字符串的长度并输出
Serial.println("String length: " + String(str.length()));
```

输出结果为：String length: 11

## 深入了解字符串长度的原理

在Arduino编程中，字符串的长度实际上就是字符串中字符的数量。一个字符占用一个字节，所以字符串的长度也可以理解为占用的字节数。在C语言中，字符串以空字符（null）结尾，这也是Arduino中的约定。当使用String对象时，length()函数会自动跳过这个空字符，所以实际上是计算出了字符串中实际的字符数量。

## 参考资料

- [Arduino官网](https://www.arduino.cc/)
- [String.length() - Arduino Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/length/)
- [What is the size of 1 char array?](https://electronics.stackexchange.com/questions/208946/what-is-the-size-of-1-char-array) 

## 了解更多Arduino编程知识

想要学习更多关于Arduino编程的知识，可以参考以下链接：

- [Arduino编程入门教程](https://www.arduino.cc/en/Guide/Introduction)
- [Arduino中国论坛](https://www.arduino.cn/)
- [Arduino开发者社区](https://forum.arduino.cc/)
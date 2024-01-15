---
title:                "输出标准错误"
html_title:           "Arduino: 输出标准错误"
simple_title:         "输出标准错误"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 为什么
为什么写入标准错误编程是重要的？因为当我们在编程过程中，难免会遇到各种问题，写入标准错误可以让我们及时发现并修复程序中的错误，提高编程效率。

## 如何操作
```Arduino
//编程示例
//将错误信息写入标准错误
Serial1.print("Error: Sensor not calibrated");
```
编程示例输出：
Error: Sensor not calibrated

## 深入了解
写入标准错误是一种调试技术，可以让我们实时监测程序中的错误信息，帮助我们更快地发现和修复代码中的问题。在Arduino中，我们可以使用Serial.print()来将错误信息输出到串口，也可以使用Serial1.print()来输出到Serial1串口。需要注意的是，写入标准错误只能在串口监视器或串口终端中查看，无法在程序中直接访问。

## 参考链接
- [使用Serial Monitor调试Arduino项目](https://www.arduino.cn/thread-87963-1-1.html)
- [Arduino官方文档](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [理解编程中的标准输出和标准错误](https://www.itread01.com/content/1500866225.html)
- [Arduino论坛](https://www.arduino.cn/forum.php)

**参考链接仅供进一步学习和参考，建议结合实际场景进行编程。**

## 相关链接
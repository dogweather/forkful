---
title:                "Arduino: 将文章的主题从英语翻译为中文：写入标准错误。"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 为什么

如果你正在编程Arduino，你可能会发现自己在做一些有趣的项目，同时也会遇到一些bug或错误。这时，通过编写到标准错误的代码，可以帮助你更容易地找到并解决这些问题。

## 如何做到

编写到标准错误的代码非常简单，只需要在你的程序中使用`Serial.println()`命令，并将错误信息作为参数传递给它。下面是一个示例代码：

```
Arduino.println("出现错误，请检查连接");
```

如果你在Arduino IDE的串口监视器中打开了“改变波特率”选项，你就可以看到这条消息。如果你的程序中发生了其他错误，你也可以通过类似的方法打印出其他错误信息。

## 深入了解

如果你想更加深入了解这个过程，你可以尝试使用`Serial.write()`命令，它可以将二进制数据打印到串口监视器。这样可以帮助你更好地理解错误信息是如何通过串口发送给电脑的。

## 了解更多

如果你想进一步学习如何使用Arduino，在下面的链接中可以找到相关的资料：

- [Arduino官方文档](https://www.arduino.cc/reference/)
- [Arduino论坛](https://forum.arduino.cc/)
- [Arduino教程和例程](https://www.arduino.cc/en/Tutorial/HomePage)

## 参考链接

- [如何在Arduino中使用Serial.println](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [如何调试Arduino程序](https://www.arduino.cc/en/Guide/Troubleshooting)
- [了解标准错误和标准输出的区别](https://arduino.stackexchange.com/questions/272/println-and-error-within-arduino)
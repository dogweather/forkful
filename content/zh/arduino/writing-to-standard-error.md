---
title:    "Arduino: 编写标准错误"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## 为什么
在编写Arduino程序时，有时候会遇到一些错误。如果我们不知道如何调试和解决这些错误，我们的程序可能无法正常运行。因此，学习如何写入标准错误是非常重要的，它可以帮助我们更有效地调试和解决程序中的问题。

## 如何
在Arduino编程中，我们可以使用 ```ArduinoprintError()``` 函数来将错误信息写入标准错误。下面是一个示例代码和输出：

```
Arduino.printError("无效的变量操作");
```
输出：
```
Error: 无效的变量操作
```

我们也可以使用 ```ArduinoprintlnError()```函数来分行打印错误信息，如下所示：

```
Arduino.printlnError("无法连接到服务器");
```
输出：
```
Error: 
无法连接到服务器
```

## 深入探讨
当我们调用 ```ArduinoprintError()``` 或 ```ArduinoprintlnError()``` 函数时，会打印出 "Error:" 开头的错误信息。这些信息会被写入标准错误，默认情况下会将其显示在串行监视器中。要在特定的板子上显示错误信息，可以在 ```Arduino.h``` 头文件中修改输出流的定义。

## 参考链接
- [Arduino官方网站](https://www.arduino.cc/)
- [如何处理Arduino错误](https://shumeipai.nxez.com/2016/07/12/arduino-error-dealing.html)
- [Arduino错误处理：打印错误信息](https://www.kanhaoyi.com/arduino-shuoming-arduino_error_absolute_error_arduino_error_relative_error.html)

## 参见
- [Arduino编程指南](https://www.arduino.cc/reference/en/)
- [Arduino标准库参考](https://www.arduino.cc/reference/zh/)
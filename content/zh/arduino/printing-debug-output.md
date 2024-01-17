---
title:                "打印调试输出"
html_title:           "Arduino: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

# 什么 & 为什么？

打印调试输出是一种程序员用来检查程序运行情况的方法。它可以帮助我们找出程序中可能存在的错误或者其他问题，从而让我们更容易地修复和优化代码。

# 如何：

```arduino
// 示例 1：打印变量的值
int num = 10;
Serial.println(num); // 输出：10

// 示例 2：打印字符串
String message = "Hello Arduino!";
Serial.println(message); // 输出：Hello Arduino!

// 示例 3：打印多个变量
float temperature = 25.6;
int humidity = 50;
Serial.println("当前温度："); // 输出：当前温度：
Serial.println(temperature); // 输出：25.6
Serial.println("当前湿度："); // 输出：当前湿度：
Serial.println(humidity); // 输出：50
```

# 深入探讨：

1. 历史背景：打印调试输出这种方法早期常用于计算机编程，随着技术的发展，现在也可以在Arduino板上实现。
2. 其他方法：除了打印调试输出，还有一些其他的方法如断点调试、单步执行等可以帮助程序员进行调试。
3. 实现细节：Arduino的Serial类中提供了一些函数用来打印调试输出，我们可以直接调用这些函数来实现打印功能。

# 参考链接：

- [Arduino文档 - Serial.println()](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)
- [打印调试输出视频教程](https://www.youtube.com/watch?v=zqF9ffSGFFA)
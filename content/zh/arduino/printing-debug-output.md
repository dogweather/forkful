---
title:                "Arduino: 打印调试输出"
programming_language: "Arduino"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

# 为什么
有时候在进行Arduino编程时，我们会遇到一些问题，比如代码中出现错误、传感器无法正常工作等等。此时，通过打印调试输出可以帮助我们更快地定位错误，从而更有效地解决问题。打印调试输出是Arduino编程中非常重要的一部分，能够提高我们的编程效率。

## 如何
打印调试输出非常简单，只需要在代码中使用```Serial.println()```或者```Serial.print()```来输出相关信息即可。下面是一个简单的示例代码：

```Arduino
int sensorValue = 0;  // 定义一个变量用来存储传感器的数值

void setup() {
  // 在串口上打开通信
  Serial.begin(9600); 
}

void loop() {
  // 读取传感器数值
  sensorValue = analogRead(A0); 

  // 在串口上输出调试信息
  Serial.print("传感器数值：");
  Serial.println(sensorValue);

  // 延时100毫秒，避免输出过于频繁
  delay(100); 
}
```

在上面的代码中，我们通过```Serial.print()```和```Serial.println()```分别输出了一条调试信息，并且在每次循环时都会将传感器的数值打印出来。通过串口监视器，我们就可以实时查看调试信息，从而判断代码是否正常运行。

## 深入了解
除了使用```Serial.print()```和```Serial.println()```外，我们还可以通过使用不同的参数来控制调试输出的格式，比如指定打印的长度和输出的进制等。具体的参数设置可以参考Arduino官方文档。

同时，我们还可以使用条件语句来限制调试输出，比如在特定的情况下才打印相关信息，从而避免过多的输出影响代码的运行效率。

# 参考链接
- [Arduino官方文档](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Serial通信介绍](https://wikipedia.org/Serial通信)
- [Serial通信原理及应用](https://www.jianshu.com/p/729e354f1fb3)

# 参见
- [如何使用Arduino调试功能](https://blog.csdn.net/qq_40389338/article/details/88422305)
- [Arduino调试技巧](https://blog.csdn.net/www_tao/article/details/8761816)
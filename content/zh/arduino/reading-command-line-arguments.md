---
title:                "Arduino: 阅读命令行参数"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 为什么

在开始学习如何读取命令行参数之前，让我们先来思考一下为什么我们会对这个话题感兴趣。读取命令行参数是一个非常实用的技能，它可以让我们的程序变得更加灵活和智能。当我们需要在不同的情况下改变程序的行为时，读取命令行参数就成为了一个必备的技能。

## 如何

要在Arduino中读取命令行参数，我们需要使用到两个函数：`Serial.available()`和`Serial.read()`。首先，我们需要在设置函数（`setup()`）中使用`Serial.begin()`来打开串口通信。然后，在主循环中使用条件语句`if`来检查串口缓冲区中是否有数据。如果有数据可用，我们可以使用`Serial.read()`来读取数据并将其存储到一个变量中。让我们来看一个简单的例子：

```Arduino
void setup() {
  Serial.begin(9600); // 打开串口通信，波特率为9600
}

void loop() {
  if (Serial.available() > 0) { // 检查串口缓冲区中是否有数据可用
    char input = Serial.read(); // 将数据读取并存入变量input
    Serial.println(input); // 打印出读取到的数据
 }
}
```

如果我们在串口终端中输入一个字符，比如字母"A"，程序就会将其读取并打印出来。这样，我们就可以通过不同的输入来控制程序的行为。

## 深入探讨

除了使用`Serial.read()`来读取单个字符之外，我们还可以使用`Serial.readString()`来读取一串字符，或者使用`Serial.parseInt()`来读取一个数字。我们也可以使用`Serial.parseFloat()`来读取一个小数。这取决于我们需要读取的数据类型。此外，在读取命令行参数时，我们还可以使用`Serial.setTimeout()`来设置等待用户输入的超时时间。这样可以避免程序一直等待用户输入而导致程序崩溃。

## 参考链接

- [Arduino官方文档：Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [读取Arduino命令行参数的教程](https://create.arduino.cc/projecthub/feilipu/reading-serial-on-the-arduino-cb62dd)
- [使用Java来读取Arduino命令行参数](https://programmingistheway.wordpress.com/2012/05/27/java-serial-comminucation-arduino-reading-string-from-serial-line/)

## 参见

- [为Arduino添加命令行参数的功能](https://forums.adafruit.com/viewtopic.php?t=70164)
- [在Arduino网络控制中使用命令行参数](https://howtomechatronics.com/tutorials/arduino/arduino-network-control-with-terminal-commands-tutorial/)
---
title:                "读取命令行参数"
html_title:           "C: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 什么和为什么？
命令行参数读取是读取并解析在命令行运行程序时传递的参数的过程。程序员经常这样做以向程序提供灵活性，使其可以在不同的场景和条件下运行。

## 怎样操作：
Arduino 并不直接支持命令行参数读取，因为它主要设计用于硬件控制。那么以串口通信为参考，我们可以创建类似的交互。

```Arduino
String inputString = "";  // 储存串口读入
bool stringComplete = false;  // 标记串口读入完成

void setup() {
  Serial.begin(9600);
  inputString.reserve(200);
}

void loop() {
  if (stringComplete) {
    Serial.println(inputString);
    inputString = "";
    stringComplete = false;
  }
}

void serialEvent() {
  while (Serial.available()) {
    char inChar = (char)Serial.read();
    inputString += inChar;
    if (inChar == '\n') {
      stringComplete = true;
    }
  }
}
```
输出结果如下:

```
hello world
hello world
```

## 深入解读
命令行参数在早期的命令行系统中帮助人们更精细地控制程序。然而，在 Arduino 中，这个概念并没有直接的实现，因为 Arduino 主要用于 与硬件设备交互。尽管如此，程序员可以使用串口通信来模拟该过程。

在实现中，我们在程序中创建一个串口事件，通过它读取字符并添加到字符串中，直到我们看到一个换行符，该符号表示输入已经完成。

作为替代方案, 你可以使用 WiFi 或者蓝牙模块来实现类似的效果，但是这些设备在处理数据上没有串口通信来得那么简单。

## 查看更多
为了更深入地了解这一主题，以下是一些资源：
1. 命令行参数在其他编程环境中的处理方法: [阅读更多](http://www.example.com)
2. Arduino 上的串口通信和事件处理: [阅读更多](http://www.example.com)
3. 使用 WiFi 或蓝牙模块作为输入参数的来源: [阅读更多](http://www.example.com)
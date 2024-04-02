---
date: 2024-01-20 17:55:36.580500-07:00
description: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570\u5141\u8BB8\u7A0B\u5E8F\u6839\
  \u636E\u7528\u6237\u8F93\u5165\u5B9E\u65F6\u4FEE\u6539\u884C\u4E3A\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u6837\u505A\uFF0C\u65E8\u5728\u63D0\u9AD8\u7075\u6D3B\u6027\uFF0C\u8BA9\
  \u540C\u4E00\u7A0B\u5E8F\u9002\u5E94\u591A\u79CD\u60C5\u51B5\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.080006-06:00'
model: gpt-4-1106-preview
summary: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570\u5141\u8BB8\u7A0B\u5E8F\u6839\
  \u636E\u7528\u6237\u8F93\u5165\u5B9E\u65F6\u4FEE\u6539\u884C\u4E3A\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u6837\u505A\uFF0C\u65E8\u5728\u63D0\u9AD8\u7075\u6D3B\u6027\uFF0C\u8BA9\
  \u540C\u4E00\u7A0B\u5E8F\u9002\u5E94\u591A\u79CD\u60C5\u51B5\u3002"
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
weight: 23
---

## What & Why? 什么及为什么？
读取命令行参数允许程序根据用户输入实时修改行为。程序员这样做，旨在提高灵活性，让同一程序适应多种情况。

## How to: 如何操作
Arduino平台通常不涉及传统意义上的命令行参数。但是，我们可以通过序列监视器处理输入作为参数。以下是一个简单的示例：

```cpp
void setup() {
  // 开启串口通信
  Serial.begin(9600);
}

void loop() {
  if (Serial.available() > 0) {
    // 读取序列监视器的输入
    String command = Serial.readStringUntil('\n');
    
    // 判断并执行命令
    if (command == "LED_ON") {
      digitalWrite(LED_BUILTIN, HIGH);
      Serial.println("LED is on");
    } else if (command == "LED_OFF") {
      digitalWrite(LED_BUILTIN, LOW);
      Serial.println("LED is off");
    } else {
      Serial.println("Unknown command: " + command);
    }
  }
}
```

示例输出：
```
LED is on
LED is off
Unknown command: LIGHT_UP
```

## Deep Dive 深入探索
在Arduino中，"命令行参数"通常指的是通过串行通信发送给Arduino的输入。虽然这与传统的命令行参数不同，但它具有相似之处，尤其是它改变程序行为的方式。

历史上，电脑程序通过命令行接收参数，以便在运行时改变其行为，无需重新编译代码。虽然Arduino不是传统的计算设备，且通常不通过命令行运行，但我们可以模仿这种行为。

除了串行通信，还可以使用网络模块（如ESP8266或ESP32）接收HTTP请求作为参数，或者使用其他类型的通讯协议（如I2C或SPI）来达到类似目的。

实施细节包括如何有效解析和处理参数，同时避免在Arduino微控制器上很常见的内存不足问题。

## See Also 查看更多
- [Arduino官方串口通信教程](https://www.arduino.cc/reference/en/language/functions/communication/serial/) - 了解更多关于Arduino串口通信的信息。
- [Arduino String类参考](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/) - 深入了解Arduino String类和相关操作。
- [Digital Pins](https://www.arduino.cc/en/Tutorial/Foundations/DigitalPins) - 如何使用Arduino的数字引脚。
- [ESP8266 WiFi 模块](https://www.arduino.cc/en/Guide/ArduinoWiFiShield) - 如何将WiFi功能添加到Arduino项目。

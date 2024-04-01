---
date: 2024-01-26 03:47:27.474290-07:00
description: "\u8C03\u8BD5\u5668\u662F\u4E00\u4E2A\u5DE5\u5177\uFF0C\u5B83\u901A\u8FC7\
  \u8BA9\u4F60\u6682\u505C\u3001\u68C0\u67E5\u5468\u56F4\u73AF\u5883\u3001\u4EE5\u53CA\
  \u4E86\u89E3\u4EE3\u7801\u5E95\u5C42\u771F\u6B63\u53D1\u751F\u4E86\u4EC0\u4E48\uFF0C\
  \u5E2E\u52A9\u4F60\u4FEE\u590D\u4EE3\u7801\u4E2D\u7684\u9519\u8BEF\u3002\u7A0B\u5E8F\
  \u5458\u4F7F\u7528\u8C03\u8BD5\u5668\u9010\u6B65\u6267\u884C\u4EE3\u7801\u3001\u68C0\
  \u67E5\u53D8\u91CF\u3001\u5E76\u7406\u89E3\u95EE\u9898\u53EF\u80FD\u51FA\u5728\u54EA\
  \u91CC\u3002"
lastmod: '2024-03-13T22:44:48.066356-06:00'
model: gpt-4-0125-preview
summary: "\u8C03\u8BD5\u5668\u662F\u4E00\u4E2A\u5DE5\u5177\uFF0C\u5B83\u901A\u8FC7\
  \u8BA9\u4F60\u6682\u505C\u3001\u68C0\u67E5\u5468\u56F4\u73AF\u5883\u3001\u4EE5\u53CA\
  \u4E86\u89E3\u4EE3\u7801\u5E95\u5C42\u771F\u6B63\u53D1\u751F\u4E86\u4EC0\u4E48\uFF0C\
  \u5E2E\u52A9\u4F60\u4FEE\u590D\u4EE3\u7801\u4E2D\u7684\u9519\u8BEF\u3002\u7A0B\u5E8F\
  \u5458\u4F7F\u7528\u8C03\u8BD5\u5668\u9010\u6B65\u6267\u884C\u4EE3\u7801\u3001\u68C0\
  \u67E5\u53D8\u91CF\u3001\u5E76\u7406\u89E3\u95EE\u9898\u53EF\u80FD\u51FA\u5728\u54EA\
  \u91CC\u3002"
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
---

## 如何操作：
使用 Arduino IDE时，你可以通过串行打印来进行调试，但这有点像用手电筒探索洞穴。要进行真正的调试，你可能希望提升你的游戏使用类似Atmel-ICE调试器这样的工具，它与Arduino环境集成。这里有使用串行进行伪调试的一个示例：

```Arduino
void setup() {
  Serial.begin(9600);
}
void loop() {
  int sensorValue = analogRead(A0);
  Serial.print("传感器值：");
  Serial.println(sensorValue);
  // 想象你这里期望是512，但得到了0。
  // 是时候检查传感器连接了
  delay(1000); // 在再次读取之前等待一秒钟
}
```
运行这段代码并打开串行监视器，你将实时看到你的传感器输出了什么。

## 深入探索
在调试器出现之前，这是一个打印语句的世界——你只能通过打印一切来猜测发生了什么。在更简单的环境或像Arduino这样的受限硬件上，使用打印进行调试仍然很常见。

替代品包括像Atmel-ICE这样的在线电路仿真器，以及像`avr-gdb`这样的软件调试工具。你可以将其与`avarice`配对，创建GDB和你的硬件之间的桥梁，这对于在芯片上进行更高级的调试非常方便。

使用调试器时，你可以设置断点来停止在某些点的执行。你可以逐行执行代码、检查内存、寄存器和变量。这让你能够精确定位问题，而不是胡乱尝试。实施调试器时，请确保你的环境正确设置——版本不匹配或配置不良的工具可能导致挫败。

## 另请参阅
准备好深入了解吗？请查阅以下内容：
- Arduino调试指南：[Arduino Debugging](https://www.arduino.cc/en/Guide/Environment#toc7)
- 设置avr-gdb的AVR Libc参考手册：[AVR Libc主页](http://www.nongnu.org/avr-libc/)

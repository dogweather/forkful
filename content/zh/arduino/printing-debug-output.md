---
title:                "打印调试输出"
date:                  2024-01-20T17:51:59.442645-07:00
model:                 gpt-4-1106-preview
simple_title:         "打印调试输出"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么？)
打印调试输出是将程序执行期间的信息显示在串行监视器上的过程。程序员这样做是为了检查变量状态、监控执行流程，以及定位错误。

## How to: (如何操作：)
```Arduino
void setup() {
  Serial.begin(9600); // 初始化串行通信
}

void loop() {
  int sensorValue = analogRead(A0); // 读取A0口模拟值
  Serial.print("Sensor value: ");
  Serial.println(sensorValue); // 打印变量值
  delay(1000); // 1秒延迟
}
```
样例输出：
```
Sensor value: 345
Sensor value: 346
Sensor value: 347
```

## Deep Dive (深入探究)
历史上，打印调试信息始于打字机和早期的计算机终端。在Arduino中，Serial对象提供了若干方法如`print()`和`println()`来发送数据到连在USB上的电脑。选择不使用串行调试的替代方案可能是使用LED指示灯或者LCD屏幕来显示状态。但在处理复杂问题时，这些方法通常不如串行输出直接和丰富。

## See Also (另请参阅)
- Arduino官方文档关于串行通信：[Arduino - Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- 关于更先进调试方法的详细讨论：[Advanced Arduino Debugging](https://create.arduino.cc/projecthub/Arduino_Genuino/advanced-arduino-debugging-af1ebb)
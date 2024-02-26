---
date: 2024-01-20 17:51:59.442645-07:00
description: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA\u662F\u5C06\u7A0B\u5E8F\u6267\u884C\
  \u671F\u95F4\u7684\u4FE1\u606F\u663E\u793A\u5728\u4E32\u884C\u76D1\u89C6\u5668\u4E0A\
  \u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u68C0\
  \u67E5\u53D8\u91CF\u72B6\u6001\u3001\u76D1\u63A7\u6267\u884C\u6D41\u7A0B\uFF0C\u4EE5\
  \u53CA\u5B9A\u4F4D\u9519\u8BEF\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:45.630928-07:00'
model: gpt-4-1106-preview
summary: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA\u662F\u5C06\u7A0B\u5E8F\u6267\u884C\
  \u671F\u95F4\u7684\u4FE1\u606F\u663E\u793A\u5728\u4E32\u884C\u76D1\u89C6\u5668\u4E0A\
  \u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u68C0\
  \u67E5\u53D8\u91CF\u72B6\u6001\u3001\u76D1\u63A7\u6267\u884C\u6D41\u7A0B\uFF0C\u4EE5\
  \u53CA\u5B9A\u4F4D\u9519\u8BEF\u3002"
title: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA"
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

---
date: 2024-01-20 17:51:59.442645-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T22:51:01.274588-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5386\u53F2\u4E0A\uFF0C\u6253\u5370\u8C03\
  \u8BD5\u4FE1\u606F\u59CB\u4E8E\u6253\u5B57\u673A\u548C\u65E9\u671F\u7684\u8BA1\u7B97\
  \u673A\u7EC8\u7AEF\u3002\u5728Arduino\u4E2D\uFF0CSerial\u5BF9\u8C61\u63D0\u4F9B\u4E86\
  \u82E5\u5E72\u65B9\u6CD5\u5982`print()`\u548C`println()`\u6765\u53D1\u9001\u6570\
  \u636E\u5230\u8FDE\u5728USB\u4E0A\u7684\u7535\u8111\u3002\u9009\u62E9\u4E0D\u4F7F\
  \u7528\u4E32\u884C\u8C03\u8BD5\u7684\u66FF\u4EE3\u65B9\u6848\u53EF\u80FD\u662F\u4F7F\
  \u7528LED\u6307\u793A\u706F\u6216\u8005LCD\u5C4F\u5E55\u6765\u663E\u793A\u72B6\u6001\
  \u3002\u4F46\u5728\u5904\u7406\u590D\u6742\u95EE\u9898\u65F6\uFF0C\u8FD9\u4E9B\u65B9\
  \u6CD5\u901A\u5E38\u4E0D\u5982\u4E32\u884C\u8F93\u51FA\u76F4\u63A5\u548C\u4E30\u5BCC\
  \u3002"
title: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA"
weight: 33
---

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

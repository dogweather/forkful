---
date: 2024-01-26 01:16:35.694802-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5047\u8BBE\u4F60\u7684Arduino\u4E0A\
  \u6709\u4E00\u4E2A\u529F\u80FD\u505A\u5F97\u592A\u591A\uFF0C\u50CF\u8FD9\u6837\uFF1A\
  ."
lastmod: '2024-04-05T22:38:47.227487-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5047\u8BBE\u4F60\u7684Arduino\u4E0A\u6709\
  \u4E00\u4E2A\u529F\u80FD\u505A\u5F97\u592A\u591A\uFF0C\u50CF\u8FD9\u6837\uFF1A."
title: "\u4EE3\u7801\u91CD\u6784"
weight: 19
---

## 如何操作：
假设你的Arduino上有一个功能做得太多，像这样：

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  // 一个做得太多的函数
  handleEverything();
}

void handleEverything() {
  // 读取传感器数据
  int sensorValue = analogRead(A0);
  // 处理传感器数据
  sensorValue = map(sensorValue, 0, 1023, 0, 255);
  // 打印传感器数据
  Serial.println(sensorValue);
  delay(500);
}
```

重构它可能看起来像是将`handleEverything()`分割成更小、更专注的函数：

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  int sensorValue = readSensorData();
  int processedValue = processSensorData(sensorValue);
  printData(processedValue);
  delay(500);
}

int readSensorData() {
  return analogRead(A0);
}

int processSensorData(int sensorValue) {
  return map(sensorValue, 0, 1023, 0, 255);
}

void printData(int data) {
  Serial.println(data);
}
```

重构后，`loop()`函数更具可读性，且每个任务都由一个专用函数处理，使代码更易于管理。

## 深入探索
从历史上看，随着敏捷和测试驱动开发（TDD）方法论的兴起，重构变得流行起来，这些方法论依赖于不断的代码改进以适应变化的需求。有各种各样的重构工具和策略 - 就像我们在Arduino示例中使用的“提取方法”技术。当您从快速原型转向稳定项目时，这是至关重要的，代码的可读性和维护性变得至关重要。

重构时，重要的是要有一套好的测试，以确保更改没有引入任何错误。在Arduino世界中，由于硬件依赖性，自动化测试并不总是直截了当的，但您仍然可以对纯逻辑部分使用单元测试或使用模拟器。

手动重构的替代方法包括使用专用的重构工具，这些工具自动识别代码异味并建议更改。然而，这些工具往往缺乏对微控制器代码的细微差别，并且可能不在Arduino开发环境中可用。

最终，重构是一种艺术，它平衡了改进代码内部结构和引入缺陷的风险。它要求您思考实现细节，如内存使用和处理器时间，尤其是由于微控制器的资源受限性质。

## 另请参阅
您可以通过马丁·福勒的开创性书籍《重构：改善既有代码的设计》深入了解重构。欲了解更多关于Arduino特定实践的信息，请查看Arduino开发论坛和社区：

- [Arduino论坛 - 编程问题](https://forum.arduino.cc/index.php?board=4.0)
- [重构大师](https://refactoring.guru/refactoring)

记住，目标是清晰、可理解的代码，未来的你和其他人将会感谢你。继续探索，保持整洁！

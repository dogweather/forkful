---
date: 2024-01-26 01:16:35.694802-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u4ECE\u5386\u53F2\u4E0A\u770B\uFF0C\u968F\
  \u7740\u654F\u6377\u548C\u6D4B\u8BD5\u9A71\u52A8\u5F00\u53D1\uFF08TDD\uFF09\u65B9\
  \u6CD5\u8BBA\u7684\u5174\u8D77\uFF0C\u91CD\u6784\u53D8\u5F97\u6D41\u884C\u8D77\u6765\
  \uFF0C\u8FD9\u4E9B\u65B9\u6CD5\u8BBA\u4F9D\u8D56\u4E8E\u4E0D\u65AD\u7684\u4EE3\u7801\
  \u6539\u8FDB\u4EE5\u9002\u5E94\u53D8\u5316\u7684\u9700\u6C42\u3002\u6709\u5404\u79CD\
  \u5404\u6837\u7684\u91CD\u6784\u5DE5\u5177\u548C\u7B56\u7565 - \u5C31\u50CF\u6211\
  \u4EEC\u5728Arduino\u793A\u4F8B\u4E2D\u4F7F\u7528\u7684\u201C\u63D0\u53D6\u65B9\u6CD5\
  \u201D\u6280\u672F\u3002\u5F53\u60A8\u4ECE\u5FEB\u901F\u539F\u578B\u8F6C\u5411\u7A33\
  \u5B9A\u9879\u76EE\u65F6\uFF0C\u8FD9\u662F\u81F3\u5173\u91CD\u8981\u7684\uFF0C\u4EE3\
  \u7801\u7684\u53EF\u8BFB\u6027\u548C\u7EF4\u62A4\u6027\u53D8\u5F97\u81F3\u5173\u91CD\
  \u8981\u3002\u2026"
lastmod: '2024-04-05T22:51:01.281214-06:00'
model: gpt-4-0125-preview
summary: "\u91CD\u6784\u65F6\uFF0C\u91CD\u8981\u7684\u662F\u8981\u6709\u4E00\u5957\
  \u597D\u7684\u6D4B\u8BD5\uFF0C\u4EE5\u786E\u4FDD\u66F4\u6539\u6CA1\u6709\u5F15\u5165\
  \u4EFB\u4F55\u9519\u8BEF\u3002\u5728Arduino\u4E16\u754C\u4E2D\uFF0C\u7531\u4E8E\u786C\
  \u4EF6\u4F9D\u8D56\u6027\uFF0C\u81EA\u52A8\u5316\u6D4B\u8BD5\u5E76\u4E0D\u603B\u662F\
  \u76F4\u622A\u4E86\u5F53\u7684\uFF0C\u4F46\u60A8\u4ECD\u7136\u53EF\u4EE5\u5BF9\u7EAF\
  \u903B\u8F91\u90E8\u5206\u4F7F\u7528\u5355\u5143\u6D4B\u8BD5\u6216\u4F7F\u7528\u6A21\
  \u62DF\u5668\u3002"
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

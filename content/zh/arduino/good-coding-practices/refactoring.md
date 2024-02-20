---
date: 2024-01-26 01:16:35.694802-07:00
description: "\u91CD\u6784\u662F\u6307\u5BF9\u4EE3\u7801\u8FDB\u884C\u91CD\u65B0\u5DE5\
  \u4F5C\u4EE5\u6539\u8FDB\u5176\u7ED3\u6784\u548C\u53EF\u8BFB\u6027\uFF0C\u4F46\u4E0D\
  \u6539\u53D8\u5916\u90E8\u884C\u4E3A\u6216\u529F\u80FD\u7684\u8FC7\u7A0B\u3002\u7A0B\
  \u5E8F\u5458\u91CD\u6784\u662F\u4E3A\u4E86\u4F7F\u4EE3\u7801\u66F4\u5E72\u51C0\u3001\
  \u66F4\u6613\u4E8E\u7406\u89E3\u548C\u66F4\u53EF\u7EF4\u62A4\uFF0C\u8FD9\u6837\u4ECE\
  \u957F\u8FDC\u6765\u770B\uFF0C\u8C03\u8BD5\u548C\u6DFB\u52A0\u65B0\u7279\u6027\u5C31\
  \u4F1A\u5C11\u5F97\u591A\u7684\u5934\u75DB\u3002"
lastmod: 2024-02-19 22:05:07.126067
model: gpt-4-0125-preview
summary: "\u91CD\u6784\u662F\u6307\u5BF9\u4EE3\u7801\u8FDB\u884C\u91CD\u65B0\u5DE5\
  \u4F5C\u4EE5\u6539\u8FDB\u5176\u7ED3\u6784\u548C\u53EF\u8BFB\u6027\uFF0C\u4F46\u4E0D\
  \u6539\u53D8\u5916\u90E8\u884C\u4E3A\u6216\u529F\u80FD\u7684\u8FC7\u7A0B\u3002\u7A0B\
  \u5E8F\u5458\u91CD\u6784\u662F\u4E3A\u4E86\u4F7F\u4EE3\u7801\u66F4\u5E72\u51C0\u3001\
  \u66F4\u6613\u4E8E\u7406\u89E3\u548C\u66F4\u53EF\u7EF4\u62A4\uFF0C\u8FD9\u6837\u4ECE\
  \u957F\u8FDC\u6765\u770B\uFF0C\u8C03\u8BD5\u548C\u6DFB\u52A0\u65B0\u7279\u6027\u5C31\
  \u4F1A\u5C11\u5F97\u591A\u7684\u5934\u75DB\u3002"
title: "\u4EE3\u7801\u91CD\u6784"
---

{{< edit_this_page >}}

## 什么 & 为什么？
重构是指对代码进行重新工作以改进其结构和可读性，但不改变外部行为或功能的过程。程序员重构是为了使代码更干净、更易于理解和更可维护，这样从长远来看，调试和添加新特性就会少得多的头痛。

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

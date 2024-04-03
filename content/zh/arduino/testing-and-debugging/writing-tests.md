---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:34.559705-07:00
description: "\u5728 Arduino \u73AF\u5883\u4E2D\u7F16\u5199\u6D4B\u8BD5\u662F\u6307\
  \u521B\u5EFA\u81EA\u52A8\u5316\u6D4B\u8BD5\u7684\u8FC7\u7A0B\uFF0C\u8FD9\u4E9B\u6D4B\
  \u8BD5\u9A8C\u8BC1\u60A8\u7684\u4EE3\u7801\u5728 Arduino \u8BBE\u5907\u4E0A\u7684\
  \u529F\u80FD\u662F\u5426\u7B26\u5408\u9884\u671F\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\
  \u505A\u662F\u4E3A\u4E86\u786E\u4FDD\u4ED6\u4EEC\u7684\u4EE3\u7801\u6309\u9884\u671F\
  \u5DE5\u4F5C\uFF0C\u51CF\u5C11\u9519\u8BEF\uFF0C\u5E76\u63D0\u9AD8\u4ED6\u4EEC\u9879\
  \u76EE\u7684\u8D28\u91CF\uFF0C\u8FD9\u5728\u8C03\u8BD5\u53EF\u80FD\u66F4\u5177\u6311\
  \u6218\u6027\u7684\u5D4C\u5165\u5F0F\u7CFB\u7EDF\u4E2D\u5C24\u5176\u91CD\u8981\u3002"
lastmod: '2024-03-13T22:44:48.065113-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Arduino \u73AF\u5883\u4E2D\u7F16\u5199\u6D4B\u8BD5\u662F\u6307\u521B\
  \u5EFA\u81EA\u52A8\u5316\u6D4B\u8BD5\u7684\u8FC7\u7A0B\uFF0C\u8FD9\u4E9B\u6D4B\u8BD5\
  \u9A8C\u8BC1\u60A8\u7684\u4EE3\u7801\u5728 Arduino \u8BBE\u5907\u4E0A\u7684\u529F\
  \u80FD\u662F\u5426\u7B26\u5408\u9884\u671F\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\
  \u662F\u4E3A\u4E86\u786E\u4FDD\u4ED6\u4EEC\u7684\u4EE3\u7801\u6309\u9884\u671F\u5DE5\
  \u4F5C\uFF0C\u51CF\u5C11\u9519\u8BEF\uFF0C\u5E76\u63D0\u9AD8\u4ED6\u4EEC\u9879\u76EE\
  \u7684\u8D28\u91CF\uFF0C\u8FD9\u5728\u8C03\u8BD5\u53EF\u80FD\u66F4\u5177\u6311\u6218\
  \u6027\u7684\u5D4C\u5165\u5F0F\u7CFB\u7EDF\u4E2D\u5C24\u5176\u91CD\u8981\u3002."
title: "\u7F16\u5199\u6D4B\u8BD5"
weight: 36
---

## 什么 & 为什么？

在 Arduino 环境中编写测试是指创建自动化测试的过程，这些测试验证您的代码在 Arduino 设备上的功能是否符合预期。程序员这样做是为了确保他们的代码按预期工作，减少错误，并提高他们项目的质量，这在调试可能更具挑战性的嵌入式系统中尤其重要。

## 如何操作：

Arduino 没有像一些其他编程环境那样内置的测试框架。但是，您可以使用第三方库，例如 `AUnit`，来对 Arduino 代码进行单元测试。AUnit 受 Arduino 内置库 `ArduinoUnit` 和 Google 的测试框架 `Google Test` 的启发。

### 使用 AUnit 的示例：

首先，通过 Arduino IDE 中的库管理器安装 AUnit：前往 Sketch > Include Library > Manage Libraries... > 搜索 AUnit 并安装它。

然后，您可以像这样编写测试：

```cpp
#include <AUnit.h>

test(ledPinHigh) {
  const int ledPin = 13;
  pinMode(ledPin, OUTPUT);
  digitalWrite(ledPin, HIGH);
  assertTrue(digitalRead(ledPin));
}

test(ledPinLow) {
  const int ledPin = 13;
  pinMode(ledPin, OUTPUT);
  digitalWrite(ledPin, LOW);
  assertFalse(digitalRead(ledPin));
}

void setup() {
  Serial.begin(9600);
  aunit::TestRunner::run();
}

void loop() {
  // Empty
}
```
将此测试上传到您的 Arduino 板后，打开串行监视器以查看测试结果。您应该看到输出，指示每个测试是通过还是失败：

```
TestRunner started on 2 test(s).
Test ledPinHigh passed.
Test ledPinLow passed.
TestRunner duration: 0.002 seconds.
TestRunner summary: 2 passed, 0 failed, 0 skipped, 0 timed out, out of 2 test(s).
```

这个简单的示例演示了如何使用 AUnit 测试 LED 引脚的状态。通过创建测试，您确认您的 Arduino 在不同条件下的行为是否符合预期。有了 AUnit，您可以编写更复杂的测试、测试套件，并享受测试超时和为更高级场景设置/拆除程序等功能。

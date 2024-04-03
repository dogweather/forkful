---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:34.559705-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Arduino \u6CA1\u6709\u50CF\u4E00\u4E9B\
  \u5176\u4ED6\u7F16\u7A0B\u73AF\u5883\u90A3\u6837\u5185\u7F6E\u7684\u6D4B\u8BD5\u6846\
  \u67B6\u3002\u4F46\u662F\uFF0C\u60A8\u53EF\u4EE5\u4F7F\u7528\u7B2C\u4E09\u65B9\u5E93\
  \uFF0C\u4F8B\u5982 `AUnit`\uFF0C\u6765\u5BF9 Arduino \u4EE3\u7801\u8FDB\u884C\u5355\
  \u5143\u6D4B\u8BD5\u3002AUnit \u53D7 Arduino \u5185\u7F6E\u5E93 `ArduinoUnit` \u548C\
  \ Google \u7684\u6D4B\u8BD5\u6846\u67B6 `Google Test` \u7684\u542F\u53D1\u3002 #."
lastmod: '2024-03-13T22:44:48.065113-06:00'
model: gpt-4-0125-preview
summary: "Arduino \u6CA1\u6709\u50CF\u4E00\u4E9B\u5176\u4ED6\u7F16\u7A0B\u73AF\u5883\
  \u90A3\u6837\u5185\u7F6E\u7684\u6D4B\u8BD5\u6846\u67B6\u3002\u4F46\u662F\uFF0C\u60A8\
  \u53EF\u4EE5\u4F7F\u7528\u7B2C\u4E09\u65B9\u5E93\uFF0C\u4F8B\u5982 `AUnit`\uFF0C\
  \u6765\u5BF9 Arduino \u4EE3\u7801\u8FDB\u884C\u5355\u5143\u6D4B\u8BD5\u3002AUnit\
  \ \u53D7 Arduino \u5185\u7F6E\u5E93 `ArduinoUnit` \u548C Google \u7684\u6D4B\u8BD5\
  \u6846\u67B6 `Google Test` \u7684\u542F\u53D1."
title: "\u7F16\u5199\u6D4B\u8BD5"
weight: 36
---

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

---
title:                "编写测试"
aliases:
- zh/arduino/writing-tests.md
date:                  2024-02-03T19:29:34.559705-07:00
model:                 gpt-4-0125-preview
simple_title:         "编写测试"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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

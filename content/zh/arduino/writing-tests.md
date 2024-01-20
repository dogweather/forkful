---
title:                "编写测试代码"
html_title:           "Arduino: 编写测试代码"
simple_title:         "编写测试代码"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么？)
编写测试是创建验证代码片段是否正确执行的脚本过程。程序员这么做是为了确保程序在修改后依然能正确运行，减少错误。

## How to: (如何操作：)
Arduino没有传统意义上的测试框架，但你可以用`Serial`输出来检查函数或类的输出：

```Arduino
void setup() {
  Serial.begin(9600);
  bool result = testAddFunction();
  Serial.print("Test add function: ");
  Serial.println(result ? "PASSED" : "FAILED");
}

void loop() {
  // 这里通常不放置测试代码
}

bool testAddFunction() {
  int expected = 5;
  int result = add(2, 3);
  return expected == result;
}

int add(int a, int b) {
  return a + b;
}
```
输出样例：
```
Test add function: PASSED
```

## Deep Dive (深入了解)
在Arduino世界，较少有传统的自动化测试和单位测试。但随着IoT项目复杂性增加，测试变得更重要。尽管Arduino IDE 没有集成测试框架，但你可以使用额外的库，例如`ArduinoUnit`或`aunit`进行更高级的测试。这些库提供了类似其他编程环境中xUnit风格的测试方式。

## See Also (延伸阅读)
- [ArduinoUnit GitHub](https://github.com/mmurdoch/arduinounit)
- [AUnit GitHub](https://github.com/bxparks/AUnit)
- [Arduino官方文档 - 串口通信](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
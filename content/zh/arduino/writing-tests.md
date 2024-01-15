---
title:                "编写测试"
html_title:           "Arduino: 编写测试"
simple_title:         "编写测试"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## 为什么

写测试是为了确保你的程序能够正确地运行。它可以帮助你检测潜在的错误，并提高程序的可靠性和稳定性。

## 怎么做

要写测试，你可以使用Arduino自带的库中的函数，例如`assert()`和`assertEquals()`来测试你的代码。你也可以使用第三方的Arduino库，比如ArduinoUnit来进行测试。以下是一个简单的测试代码的例子：

```Arduino
#include <ArduinoUnit.h> //导入测试库

unittest(basicTest) { //定义一个测试单元
  int a = 5;
  int b = 7;
  assertEquals(a + b, 12); //测试a加b是否等于12
}

unittest_main() //运行所有的测试单元
```

运行测试后，如果所有的测试通过，那么输出就会是`OK`。如果有测试失败，它会告诉你具体的哪个测试出错了。

## 深入了解

写测试可以帮助你更有效地调试和修复代码，尤其是当你的程序变得更加复杂的时候。它也可以帮助你避免在未来的修改中引入新的错误。另外，通过使用Arduino's Serial Monitor来输出测试结果，你可以更加方便地查看测试运行的结果。

## 参考链接

- [Arduino官方文档](https://www.arduino.cc/reference/en/)
- [ArduinoUnit官方文档](https://github.com/mmurdoch/arduinounit)
- [Arduino测试教程](https://create.arduino.cc/projecthub/​)
- [Arduino代码调试技巧](https://www.instructables.com/id/Arduino-Debugging-Tricks/)
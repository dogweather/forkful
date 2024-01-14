---
title:                "Arduino: 编写测试"
programming_language: "Arduino"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/writing-tests.md"
---

{{< edit_this_page >}}

为什么：测试是软件开发中不可或缺的步骤。它可以帮助我们发现并解决潜在的错误，提高代码的质量和可靠性。

如何：编写测试代码的实际步骤如下所述：

首先，我们需要定义一个测试类，以确保我们的程序以后仍然有效。例如：

```Arduino
class TestClass{
  public:
  void runTest(){
    //测试代码将在这里被运行
  }
};
```

然后，我们可以在测试类中定义不同的测试方法来测试不同的功能。例如：

```Arduino
void testSum(){
  int result = sum(2, 3);
  assertEquals(5, result);
}
```

最后，在我们的程序中引入测试类并运行测试代码。例如：

```Arduino
TestClass tester;
tester.runTest();
```

运行测试代码后，我们可以在串口监视器中看到每个测试方法的输出结果，从而判断程序是否按预期执行。

深入了解：编写测试代码时，我们应该关注的主要方面是代码覆盖率。代码覆盖率指的是测试代码所覆盖的程序代码的百分比。通常，我们应该尽可能地提高代码覆盖率来确保程序的可靠性。

另一个重要方面是使用断言来判断测试结果是否符合预期。断言是一种可以在测试代码中使用的语句，它可以根据我们设定的预期结果来判断程序是否执行正确。如果断言失败，它将在串口监视器中输出错误信息，指出程序出现了问题。

另外，我们应该在编写程序的同时也写入测试代码，这样可以更早地发现和解决潜在的问题，从而减少后期修改的工作。

参考链接：

[Arduino官方文档](https://www.arduino.cc/reference/)

[如何为Arduino进行单元测试](https://www.ionos.com/digitalguide/server/tools/how-to-test-your-arduino-code/)

[覆盖率测试工具Codecov](https://docs.codecov.io/docs/arduino)

[断言的用法和示例](https://www.arduino.cc/en/reference/assert)

[如何写出高质量的代码：测试驱动的开发](https://www.arduino.cc/en/Hacking/TestDrivenDevelopment)

与此同时：

[注意事项：如何避免常见的Arduino编程错误](https://www.arduino.cc/en/Guide/Troubleshooting)
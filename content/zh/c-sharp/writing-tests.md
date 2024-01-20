---
title:                "编写测试。"
html_title:           "C#: 编写测试。"
simple_title:         "编写测试。"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

##
什么是写测试？
写测试是编写代码的一部分，并且旨在验证代码的正确性。它是一种良好的实践，可以帮助程序员发现并修复他们代码中的错误。

为什么程序员要写测试？
写测试可以帮助程序员在开发过程中发现代码中的错误，从而提高代码质量和可靠性。它也可以帮助开发团队更有效地协作和沟通。

如何进行写测试？
以下是一个使用C#语言的简单示例来演示如何写测试：
```
using System;

namespace Calculator
{
    class Calculator
    {
        public int Add(int num1, int num2)
        {
            return num1 + num2;
        }

        static void Main(string[] args)
        {
            Calculator calc = new Calculator();

            int result = calc.Add(2, 3);
            Console.WriteLine(result);
        }
    }
}
```
输出：5

深入了解
写测试是一种测试驱动开发（TDD）的实践方法中的一部分。它的目的是通过编写测试来指导代码的开发，从而提高代码的质量和可靠性。除了编写测试，还有一些其他的测试方法，例如集成测试和验收测试。

相关链接
更多关于TDD的信息，请查看以下链接：
- [Exploring TDD in C#](https://www.pluralsight.com/guides/exploring-test-driven-development-c-sharp)
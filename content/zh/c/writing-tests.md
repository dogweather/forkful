---
title:                "C: 编写测试"
programming_language: "C"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/writing-tests.md"
---

{{< edit_this_page >}}

## 为什么

编写测试代码是软件开发过程中至关重要的一步。它可以帮助您发现潜在的错误和缺陷，并确保您的代码在各种情况下都能正常工作。写测试代码还可以提高代码的可读性和可维护性。

## 如何做

为了向大家展示如何编写测试代码，我们将使用 C 语言作为示例。假设我们有一个简单的程序，需要将两个整数相加并返回结果。

```C
#include <stdio.h>

int add(int num1, int num2) {
    return num1 + num2;
}

int main() {
    int result = add(5, 10);
    printf("Result: %d\n", result);
    return 0;
}
```

输出应为: `Result: 15`

现在，我们将为这个简单的程序编写测试代码来验证它是否按预期工作。

```C
#include <stdio.h>

int add(int num1, int num2) {
    return num1 + num2;
}

// 测试函数
void test() {
    int result = add(5, 10);
    
    // 断言
    if (result == 15) {
        printf("Test passed!\n");
    } else {
        printf("Test failed\n");
    }
}

int main() {
    test();
    return 0;
}
```

输出应为: `Test passed!`

从输出结果可以看出，我们的测试代码成功验证了 add() 函数的功能。

## 深入探讨

编写测试代码可以帮助我们更早地发现问题，并且随着代码的增长，这种方法也更加高效。通过不断地添加测试代码，我们可以更有信心地修改和优化代码，而不用担心影响现有的功能。

此外，编写测试代码还可以帮助我们更好地理解代码和预期的结果。在撰写测试用例时，我们需要考虑各种可能的情况，这有助于增强我们对代码的理解。

最后，编写测试代码还可以帮助我们避免不必要的重生错误。当我们在修改代码时，我们可以运行测试代码来确保我们的修改没有破坏原有的功能。

## 参考

- [C语言教程](https://www.runoob.com/cprogramming/c-programming-tutorial.html)
- [简单的C语言测试框架](https://github.com/alexei-led/pumba)
- [单元测试入门教程](https://www.jianshu.com/p/328f3c6600b4)

## 参见

- [编写测试代码的重要性](https://techcrunch.com/2014/03/24/the-importance-of-writing-tests/)
- [测试代码应该覆盖多少？](https://stackoverflow.com/questions/484265/how-much-code-coverage-is-enough)
- [测试驱动开发简介](https://www.ibm.com/developerworks/cn/rational/bdd/)
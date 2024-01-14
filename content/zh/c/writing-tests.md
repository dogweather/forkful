---
title:    "C: 编写测试"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/c/writing-tests.md"
---

{{< edit_this_page >}}

## 为什么会编写测试？

在编程中，测试是非常重要的一步。它们可以帮助我们检查和验证自己编写的代码是否正确，并且可以在我们进行更改时提供反馈。编写测试也可以帮助我们节省时间和精力，因为它们可以帮助我们快速测试代码并发现潜在的问题。

## 如何编写测试

编写测试的第一步是确定我们要测试的功能或代码。然后，我们可以使用C语言中的断言(assertions)来编写测试代码。断言是一种用于验证某个条件是否满足的表达式。接下来，我们使用测试框架来运行我们编写的测试代码。一个常用的测试框架是Unity，它提供了一些方便的宏来帮助我们编写测试代码。

下面是一个简单的测试例子，我们将测试一个名为`add`的函数，它可以将两个整数相加并返回结果：

```C
#include <stdio.h>
#include <assert.h>
#include "unity.h"

/* 待测试的函数 */
int add(int x, int y) {
  return x + y;
}

/* main函数，使用Unity测试框架来运行测试 */
int main() {
  /* 添加测试代码 */
  printf("====== 测试add函数 ======\n");
  /* 第一个参数为测试名称，第二个参数为预期结果 */
  TEST_ASSERT_EQUAL(2, add(1, 1));
  TEST_ASSERT_EQUAL(5, add(2, 3));
  TEST_ASSERT_EQUAL(-4, add(0, -4));
  /* 运行所有测试并显示结果 */
  UNITY_END();
}
```

测试输出如下：

```
====== 测试add函数 ======
.
----------------------------------------------------------------------
Ran 1 test in 0.000s

OK
```

如果所有测试都通过，我们会看到`OK`的提示，这意味着我们的代码可以正确地将两个整数相加并返回正确的结果。如果有一个或多个测试失败，我们将会在输出中看到相应的错误信息，从而可以定位并修复代码中的问题。

## 深入了解编写测试

除了基本的使用方法，编写测试还有一些深层次的技巧和概念。首先，我们可以使用不同的断言来测试不同的条件，比如`TEST_ASSERT_LESS_THAN()`来测试某个值是否小于另一个值。其次，我们可以使用`TEST_IGNORE()`来忽略某个测试，比如当我们正在进行代码重构时，暂时忽略某个测试可能更方便。此外，我们还可以为我们的测试添加一些标签，比如`TEST_GROUP`和`TEST_SETUP`，来更好地组织我们的测试代码。

最后，编写测试也可以帮助我们更好地理解我们的代码。通过编写测试，我们可以深入了解代码的每一行都在做什么，以及它们是如何一起运行的。我们也可以使用测试来探索一些不常用的情况，从而发现并修复潜在的问题。

## 参考链接

* Unity测试框架：https://github.com/ThrowTheSwitch/Unity
* C语言断言(assertions)：https://www.tutorialspoint.com/c_standard_library/assert_h.htm
* 深入理解编写测试的意义：https://www.codementor.io/@nickmccullum/why-writing-tests-is-important-for-your-developer-career-xoh1lwxuo
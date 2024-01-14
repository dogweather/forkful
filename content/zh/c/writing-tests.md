---
title:                "C: 编写测试"
simple_title:         "编写测试"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/writing-tests.md"
---

{{< edit_this_page >}}

# 为什么写测试？

每个程序员都知道写测试是一个重要的过程，但是很多人都会忽略它。但是，写测试有很多好处。首先，它可以帮助我们识别程序中的错误和漏洞。通过运行测试，我们可以确保我们的代码可以正确地运行，并且能够在未来进行修改和维护。

另外，写测试可以帮助我们构建更可靠的代码。通过编写测试，我们可以更好地了解我们的代码如何与其他部分交互，从而可以更有效地进行调试和修复错误。

总的来说，编写测试可以提高我们的代码质量，并帮助我们节省宝贵的时间和精力。

# 如何写测试？

对于C语言，写测试需要使用一个称为“断言”的工具。断言是一个布尔语句，它可以验证一些条件是否为真。如果条件为真，则断言没有任何效果。但是如果条件为假，则断言会抛出一个错误，表明代码中存在问题。

让我们来看一个例子：

```C
#include <assert.h>

int divide(int a, int b) {
  // 断言，如果b为0，则抛出错误
  assert(b != 0);
  return a / b;
}

int main() {
  int result = divide(10, 2);
  // 断言，如果结果不是5，则抛出错误
  assert(result == 5);

  return 0;
}
```

在上面的示例中，我们使用`assert`函数来确保除数不为零，并且通过比较结果来验证程序是否正常工作。

# 深入了解测试

当涉及到编写测试时，有一些最佳实践值得我们注意。首先，每个函数应该至少有一个相关的测试。这可以帮助我们找出可能存在的潜在问题。

其次，测试应该涵盖尽可能多的边界条件，以及正常的输入和异常输入。这可以帮助我们确保代码在各种情况下都能正常工作。

另外，我们应该定期运行测试，以确保代码在任何更改后都能正确运行。最后，我们应该编写易于阅读和维护的测试案例，这样可以帮助我们更快地定位和修复问题。

查看下面的链接，以获取更多关于如何编写好的测试的信息。

# 参考链接

- [JUnit:手工编码测试](https://learn.mairie-at.fr/codewars/best-practices-for-writing-tests)
- [测试最佳实践：从单元测试到黄金测试到测试驱动开发](http://blog.codinglabs.org/articles/test-best-practices.html)
- [谷歌测试官方教程](https://developers.google.com/edu/testing/overview)
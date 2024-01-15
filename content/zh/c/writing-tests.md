---
title:                "编写测试"
html_title:           "C: 编写测试"
simple_title:         "编写测试"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/writing-tests.md"
---

{{< edit_this_page >}}

# 为什么要写测试？

编写测试在编程中非常重要，它可以帮助我们发现和修复潜在的错误，从而提高代码的质量和可靠性。如果我们不写测试，可能会很难发现并修复代码中的问题，最终导致出现严重的bug。

# 怎样写测试？

在C语言中，我们可以使用```assert()```语句来编写测试。这个语句需要两个参数，第一个是一个表达式，第二个是一个错误消息。如果表达式为false，测试就会失败并打印出错误消息。

下面是一个简单的例子：

```C
#include <assert.h>

// 定义一个函数，返回两个数字的和
int sum(int a, int b){
    return a + b;
}

int main(){
    // 测试函数sum()是否返回正确的结果
    assert(sum(2, 3) == 5); // 如果表达式为false，测试就会失败并打印出错误消息
    return 0;
}
```

运行上面的代码，如果测试通过，我们将看到屏幕上没有任何输出；如果测试失败，将会打印出一条错误消息。这样，我们就可以及时发现代码中的问题，并进行修复。

# 深入了解写测试

除了使用```assert()```语句外，我们还可以使用其他的测试框架，如[CUnit](http://cunit.sourceforge.net/)或[Unity](https://github.com/ThrowTheSwitch/Unity)。这些框架可以为我们提供更多的功能和灵活性，使得编写测试更加方便和高效。

另外，编写测试时应该遵循一些原则，如单一职责原则，每个测试应该只测试一个功能点；可读性原则，测试代码应该易于读懂和理解等。

# 参考链接

- [C语言中写测试的重要性](https://www.tutorialspoint.com/codingground.htm)
- [如何在C语言中编写简单的测试](https://linux.die.net/man/3/assert)

# 参见

- [assert()函数的官方文档](https://www.ibm.com/docs/en/zos-basic-skills?topic=functions-assert-explained)
- [CUnit框架的官方网站](http://cunit.sourceforge.net/)
- [Unity框架的GitHub页面](https://github.com/ThrowTheSwitch/Unity)
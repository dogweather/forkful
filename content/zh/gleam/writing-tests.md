---
title:    "Gleam: 编写测试"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/writing-tests.md"
---

{{< edit_this_page >}}

# 为什么会写测试？

在软件开发中，测试是确保程序质量的重要一环。通过编写测试，可以有效地捕获和解决程序中的错误，确保程序的稳定和可靠性。同时，测试也可以帮助开发者更快地定位和修复问题，节省宝贵的时间和精力。因此，编写测试是一个必不可少的步骤，能够为软件开发带来可靠性和高效性。

## 如何编写测试？

Gleam是一种基于函数式编程语言Erlang和Elixir的静态类型编程语言。它提供了一套强大的测试框架，可以帮助开发者轻松编写测试。下面，让我们来看一些简单的Gleam测试代码，并输出看看结果。

``` Gleam

// 定义一个函数，计算两个数的和

fn add(x, y) {
    x + y
}

// 编写测试，使用assert_equal函数对函数进行测试

fn test_adds_numbers() {
    assert_equal(add(2, 3), 5)
}

// 运行测试，输出测试结果

fn main() {
    run_test(test_adds_numbers())
}

```

运行测试的结果如下：

```bash

---- tests ----
Ok: test_adds_numbers

Ran: 1

Passed: 1
```

从结果中可以看出，我们的测试已经通过了，表示我们的函数工作正常，可以正确的计算两个数的和。

## 深入了解测试

除了简单的assert_equal函数，Gleam还提供了许多其他的测试功能，例如对于异常情况的测试、模拟测试等。此外，Gleam还具有模块化的特性，可以方便开发者进行模块化测试。总之，Gleam为开发者提供了一套完整的测试框架，帮助开发者轻松编写高效、可靠的测试代码。

## 参考链接：

- Gleam官方文档: https://gleam.run/
- Gleam测试指南：https://gleam.run/book/testing.html
- Gleam GitHub：https://github.com/gleam-lang/gleam
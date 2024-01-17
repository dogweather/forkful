---
title:                "编写测试"
html_title:           "Elm: 编写测试"
simple_title:         "编写测试"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/writing-tests.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

写测试是指编写代码来测试你的程序功能是否正常。程序员会这么做是因为编写测试可以帮助他们更快地发现和解决代码中的错误，从而提高代码质量。

## 如何：

```Elm
-- 假设我们要测试一个函数，计算一个数字的平方
square : Int -> Int
square x =
  x * x


-- 现在我们可以使用 elm-test 库来编写测试用例
import Expect
import Test exposing (..)

tests =
  describe "Square function test" [
    test "should return 4 when input is 2" <|
      \_ -> Expect.equal (square 2) 4,
    test "should return 25 when input is 5" <|
      \_ -> Expect.equal (square 5) 25
  ]
```

运行测试：

```
> tests
Passed    Square function test should return 4 when input is 2
Passed    Square function test should return 25 when input is 5
```

## 深入探讨：

编写测试是一种测试驱动开发（Test-Driven Development）的实践，它在很大程度上可以减少代码中的bug，并且可以帮助程序员更早地发现问题。除了编写单元测试，还有一些其他的测试方法，比如集成测试和端到端测试。与写测试相比，人工测试往往更容易出错，因此编写测试是一种有效的测试程序的方法。

## 参考资料：

- [Elm官方文档 - 测试](https://guide.elm-lang.org/testing/)
- [测试驱动开发（TDD）介绍 - 阮一峰的网络日志](https://www.ruanyifeng.com/blog/2013/01/what_is_tdd.html)
- [关于测试的各种方法的比较 - Medium](https://medium.com/@kentbeck_7670/technical-debt-terminology-f6267088c11#.orge3mzcp)
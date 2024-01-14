---
title:    "Elm: 编写测试"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/elm/writing-tests.md"
---

{{< edit_this_page >}}

## 为什么

为什么要编写测试呢？测试在编程过程中起着非常重要的作用。它可以确保我们的代码在运行时能够如预期般顺利运行，避免出现意想不到的bug，提高代码质量和可维护性。

## 如何做

首先，我们需要在代码中引入测试框架，比如elm-test。然后，我们可以使用各种断言函数来判断代码的输出结果是否符合我们的预期。例如：

```Elm
import Expect
import Test exposing (..)

square x = x * x

tests =
    describe "Square function"
        [ test "should square positive numbers" <|
            \() ->
                Expect.equal (square 5) 25
        , test "should return 0 for 0 input" <|
            \() ->
                Expect.equal (square 0) 0
        ]

main = beginnerProgram
    { model = tests
    , update = always
    , view = always noContent
    }
```

其中，我们通过导入Expect模块，使用equal函数来断言函数计算结果是否等于预期值。我们还可以使用其他断言函数来测试各种情况，比如Expect.greater等。

当我们运行这段代码时，如果所有测试通过，控制台会输出一个绿色的“OK”的字样，表示测试成功。

## 深入了解

编写测试不仅仅是为了简单地检查代码输出是否正确。通过编写详细的测试用例，我们可以更加深入地了解我们的代码内部实现逻辑。同时，当我们在后期对代码进行更改时，测试也能帮助我们发现潜在的bug，并及时修复，避免对现有功能的破坏。

除了单元测试，我们还可以编写集成测试来验证整个应用程序的功能是否正常。这些不仅仅是为了保证代码质量，也能帮助我们更好地理解和改进我们的代码。

## 查看更多

- [Elm官方文档-测试](https://guide.elm-lang.org/testing/)
- [Elm-test文档](https://package.elm-lang.org/packages/elm-explorations/test/latest/)
- [从零开始学习Elm-第三部分：编写测试](https://www.zhihu.com/column/p/1434539301061221376)
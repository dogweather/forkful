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

# 为什么要写测试？

写测试的主要原因是为了提高代码的质量和可靠性。通过编写测试，可以确保代码的功能正常，可以及早发现潜在的问题，并且可以帮助开发人员更快地排除错误。

## 如何编写测试？

为了编写有效的测试，首先需要在项目中安装 [Elm-Test](https://github.com/elm-explorations/test)。安装完成后，可以使用以下命令创建测试文件：

```Elm
elm-test init
```

测试文件将被放置在 `tests/` 目录下，并且以 `Tests.elm` 作为文件名。在测试文件中，可以使用 `Test` 模块中的函数来编写测试用例。例如，下面是一个简单的测试用例，测试 `String` 模块中的 `contains` 函数：

```Elm
import Test exposing (Test, describe, test)
import String exposing (contains)


containsTest : Test
containsTest =
    describe "String.contains function"
        [ test "should return True when the substring is contained in the string" <|
            \_ ->
                contains "Elm" "Elm is awesome"

        , test "should return False when the substring is not contained in the string" <|
            \_ ->
                not (contains "Hello" "Hi there")
        ]
```

在测试文件中，使用 `test` 函数来定义测试用例，并使用匿名函数来定义测试逻辑。在上面的示例中，第一个测试用例断言 `contains "Elm" "Elm is awesome"` 应该返回 `True`，而第二个测试用例断言 `contains "Hello" "Hi there"` 应该返回 `False`。通过这种方式，可以针对函数的不同输入进行测试，并验证其预期的输出。

要运行测试，可以使用以下命令：

```Elm
elm-test
```

测试结果将显示在命令行中，如果测试通过，会显示 `PASS`，如果测试失败，会显示 `FAIL` 并给出失败的具体信息。可以使用 `-f` 标志来指定测试文件，以便只运行特定的测试。

## 深入了解测试

编写有效的测试需要一定的技巧和经验。以下是一些有用的技巧：

- 使用 `expect` 函数来比较实际结果和预期结果，可以使用 `==` 或 `=/=` 来比较两个值。
- 使用 `Expecations` 模块中的函数来检查结果是否符合特定的要求，例如 `Expect.equal`、`Expect.lessThan` 等。
- 使用 `Test.label` 函数来为测试用例添加标签，这样可以更容易地识别失败的测试用例。
- 使用 `Test.fuzz` 函数来生成随机输入来测试函数的边界情况。

更多关于编写测试的技巧和指导，请参阅 [Elm-Test 文档](https://package.elm-lang.org/packages/elm-explorations/test/latest/)。

# 参考

- [Writing Tests with Elm-Test](https://elmprogramming.com/writing-tests-with-elm-test.html)
- [Elm-Test 官方文档](https://package.elm-lang.org/packages/elm-explorations/test/latest/)
- [Elm-Test 仓库](https://github.com/elm-explorations/test)
- [Elm 官方文档](https://guide.elm-lang.org/)
---
title:                "编写测试"
aliases:
- /zh/elm/writing-tests/
date:                  2024-02-03T19:30:28.987127-07:00
model:                 gpt-4-0125-preview
simple_title:         "编写测试"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么?

在 Elm 中编写测试包括编写测试用例来验证你的 Elm 代码的正确性，确保它的行为符合预期。程序员这样做是为了尽早发现错误，简化维护，并提高他们的应用程序的质量和可靠性。

## 如何做:

Elm 使用 `elm-explorations/test` 包来编写单元和模糊测试。开始时，先将此包添加到你的项目中：

```elm
elm install elm-explorations/test
```

创建一个测试文件，比如 `tests/ExampleTest.elm`，并引入测试模块。这里有一个简单的测试，用来验证函数 `add : Int -> Int -> Int`：

```elm
module ExampleTest exposing (..)

import Expect
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "一个简单的加法函数"
        [ test "将 2 和 3 相加得到 5" <| 
            \_ -> add 2 3 |> Expect.equal 5
        ]

```

要运行你的测试，你需要 `elm-test`：

```shell
npm install -g elm-test
elm-test
```

这将会编译你的测试并在终端打印结果。对于上面的示例，输出应该是这样的：

```
测试运行通过

持续时间：42毫秒
通过：  1
失败：  0
```

对于更复杂的示例，假设你想对 `add` 函数进行模糊测试，以确保它正确处理广泛的整数输入。你将按照以下方式修改你的 `ExampleTest.elm`：

```elm
module ExampleTest exposing (..)

import Expect
import Fuzz exposing (int)
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "使用模糊测试测试 add"
        [ fuzz int "通过随机整数模糊测试 add" <| 
            \int1 int2 -> add int1 int2 |> Expect.equal (int1 + int2)
        ]
```

再次运行 `elm-test` 来查看模糊测试的效果。输出会因随机输入而异，但成功的测试将表明没有失败：

```
测试运行通过

持续时间：183毫秒
通过：  100
失败：  0
``` 

这些示例展示了如何在 Elm 中使用 `elm-explorations/test` 包编写和运行简单的单元和模糊测试。测试是开发过程中的重要部分，有助于确保你的 Elm 应用程序可靠并保持高质量。

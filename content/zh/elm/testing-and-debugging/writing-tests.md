---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:28.987127-07:00
description: "\u5728 Elm \u4E2D\u7F16\u5199\u6D4B\u8BD5\u5305\u62EC\u7F16\u5199\u6D4B\
  \u8BD5\u7528\u4F8B\u6765\u9A8C\u8BC1\u4F60\u7684 Elm \u4EE3\u7801\u7684\u6B63\u786E\
  \u6027\uFF0C\u786E\u4FDD\u5B83\u7684\u884C\u4E3A\u7B26\u5408\u9884\u671F\u3002\u7A0B\
  \u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u5C3D\u65E9\u53D1\u73B0\u9519\u8BEF\
  \uFF0C\u7B80\u5316\u7EF4\u62A4\uFF0C\u5E76\u63D0\u9AD8\u4ED6\u4EEC\u7684\u5E94\u7528\
  \u7A0B\u5E8F\u7684\u8D28\u91CF\u548C\u53EF\u9760\u6027\u3002"
lastmod: '2024-03-13T22:44:47.676609-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Elm \u4E2D\u7F16\u5199\u6D4B\u8BD5\u5305\u62EC\u7F16\u5199\u6D4B\u8BD5\
  \u7528\u4F8B\u6765\u9A8C\u8BC1\u4F60\u7684 Elm \u4EE3\u7801\u7684\u6B63\u786E\u6027\
  \uFF0C\u786E\u4FDD\u5B83\u7684\u884C\u4E3A\u7B26\u5408\u9884\u671F\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u5C3D\u65E9\u53D1\u73B0\u9519\u8BEF\uFF0C\
  \u7B80\u5316\u7EF4\u62A4\uFF0C\u5E76\u63D0\u9AD8\u4ED6\u4EEC\u7684\u5E94\u7528\u7A0B\
  \u5E8F\u7684\u8D28\u91CF\u548C\u53EF\u9760\u6027\u3002."
title: "\u7F16\u5199\u6D4B\u8BD5"
weight: 36
---

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

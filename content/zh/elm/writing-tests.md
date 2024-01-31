---
title:                "编写测试代码"
date:                  2024-01-19
html_title:           "Arduino: 编写测试代码"
simple_title:         "编写测试代码"

category:             "Elm"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么要这么做？)
编写测试是创建用来验证代码正确性的程序。这样做帮助开发者捕捉bug，确保软件质量。

## How to: (如何操作：)
Elm中写测试，用`elm-test`库。先安装`elm-test`:

```shell
elm install elm-explorations/test
```

写个简单测试：

```elm
import Expect exposing (equal)
import Test exposing (test)

test "Addition works correctly" <|
    \_ -> 2 + 2 |> equal 4
```

运行测试，看到输出：

```shell
TEST RUN PASSED

Duration: 42 ms
Passed:   1
Failed:   0
```

## Deep Dive (深入了解)
- **历史背景**: Elm的测试库是基于Haskell的QuickCheck。
- **替代方案**: 其他前端框架如React使用Jest或Mocha。Elm选择`elm-test`因为它和Elm语言紧密整合。
- **实现细节**: `elm-test`运行时，它转换Elm代码到JavaScript，然后在Node.js环境或浏览器中运行。

## See Also (另请参阅)
- Elm测试介绍: [https://package.elm-lang.org/packages/elm-explorations/test/latest](https://package.elm-lang.org/packages/elm-explorations/test/latest)

---
title:                "Elm: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么

生成随机数在编程中经常被用到，它可以帮助我们创造具有随机性的数据，使程序更加有趣和真实。例如，游戏中的随机生成的地图、抽奖活动中的随机中奖、还有模拟实验中的随机设定等等。在本文中，我们将学习如何在 Elm 中生成随机数，让我们的程序更加生动有趣。

## 如何做

要在 Elm 中生成随机数，首先我们需要引入 `Random` 模块。然后，我们可以使用 `generator` 函数来定义一个生成随机数的函数，它需要一个返回随机数的核心函数作为参数。下面是一个简单的例子：

```Elm
import Random exposing (..)

getRandomInt: Int 
getRandomInt = 
    Random.int 1 10
```

上面的代码意思是定义一个名为`getRandomInt`的函数，它会在 1 到 10 之间随机返回一个整数。你可以根据自己的需求修改 `Random.int 1 10` 部分来生成不同范围的随机数。

除了整数，我们还可以生成 `Bool`、`Char`、`Float` 等各种类型的随机数。如果想要生成自定义数据类型的随机数，我们可以使用 `map` 函数将随机数转换为我们想要的类型。例如，我们可以生成一个由 1 到 10 之间的整数构成的列表：

```Elm
import Random exposing (..)
import List exposing (..)
import Array exposing (..)

getRandomList: Int -> List Int 
getRandomList n = 
    Random.list n (Random.int 1 10)

-- 随机生成10个整数组成的列表
myList: List Int 
myList = getRandomList 10 
```

## 深入探讨

在 Elm 中，生成随机数的核心函数是 `generator`，它接受一个随机数生成器，并返回一个元组，其中包括一个生成的随机数和一个新的随机生成器。通过这种方式，我们可以保证每次生成的随机数都是独立的且不重复的。此外，`Random` 模块还提供了许多有用的辅助函数，方便我们生成不同类型的随机数。

在实际应用中，我们还可以利用 `Random` 模块和其他 Elm 库来生成更加复杂的随机数据，例如生成随机姓名、邮件地址、图片等。这些随机数据可以用于测试、填充表单数据等场景，为我们的程序增添更多的随机性和创意。

## 参考链接

- Elm 官方文档：https://elm-lang.org/docs/random
- Elm Random 模块：https://package.elm-lang.org/packages/elm/random/latest/
- Elm List 模块：https://package.elm-lang.org/packages/elm/core/latest/List
- Elm Array 模块：https://package.elm-lang.org/packages/elm/core/latest/Array
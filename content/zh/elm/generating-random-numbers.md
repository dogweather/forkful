---
title:                "生成随机数"
date:                  2024-01-20T17:48:48.417826-07:00
model:                 gpt-4-1106-preview
simple_title:         "生成随机数"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
生成随机数就是创建不可预测的数字，程序员们需要这样做来处理游戏、模拟和安全性等方面的问题。

## How to: (如何操作：)
```Elm
import Random

-- 生成一个随机数生成器
randomGenerator : Random.Generator Int
randomGenerator = Random.int 1 100

-- 用随机数更新模型
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Generate ->
            ( model, Random.generate NewRandomNumber randomGenerator )
```
上面代码会生成一个1到100之间的随机整数。

## Deep Dive (深入探索)
历史上，生成伪随机数是计算机科学中的一个难题。Elm使用了伪随机数生成器（PRNG），但它跟大多数语言有所不同，因为它是纯函数式的。而通常，你可能会接触到的种子生成、线性同余发生器或者梅森旋转算法等更为复杂的系统。Elm中，随机数的生成需要处理结果类型 Msg，这保证了随机性的同时，还能维持Elm的函数式特性。

## See Also (另请参阅)
- Elm官方随机数库文档: [Random - Elm package](https://package.elm-lang.org/packages/elm/random/latest/)
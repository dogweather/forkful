---
title:                "Elm: 生成随机数"
programming_language: "Elm"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么使用 Elm 生成随机数

在编程中，有时我们需要生成随机数来模拟真实世界的情况，或者用于游戏或其他应用程序中。在 Elm 中，我们可以轻松地通过内置的函数来生成随机数，并且这些随机数是安全和可靠的。接下来，让我们来看看如何在 Elm 中生成随机数吧！

## 如何生成随机数

首先，我们需要导入内置的 Random 包。然后，我们可以使用 `Random.generate` 函数来生成一个随机数，它需要一个 generator 作为参数。下面是一个简单的例子：

```
Elm
Random.generate (Random.int 1 10)
```

以上代码将生成一个介于 1 到 10 之间的整数。我们也可以使用其他类型的随机数生成器，比如 `float` 和 `bool`。下面是一个使用 `float` 生成随机数的例子：

```
Elm
Random.generate (Random.float 0 1)
```

以上代码将生成一个介于 0 到 1 之间的浮点数。我们也可以自定义生成器，来生成符合特定需求的随机数。例如，我们可以创建一个生成 1 到 100 之间的偶数的自定义生成器：

```
Elm
evenIntGenerator : Random.Generator Int
evenIntGenerator =
    Random.int 1 50
        |> Random.map (\n -> n * 2)

Random.generate evenIntGenerator
```

以上代码将生成一个介于 1 到 100 之间的偶数。我们可以通过组合不同的生成器，来生成更加复杂的随机数。

## 深入了解生成随机数

在 Elm 中，随机数是通过生成器(generator)来产生的。生成器是一个函数，它接受一个随机数种子(seed)作为参数，并返回一个包含产生的随机数和新的种子的元组。这样的机制保证了每次调用生成器时，都会得到一个新的随机数。

此外，编写的生成器应该是“纯函数”（pure function）。也就是说，它们不会产生副作用并且对于给定的输入始终返回相同的输出。这样可以确保我们生成的随机数是可靠和安全的。

## 查看更多

如果您对 Elm 中生成随机数的更多细节感兴趣，可以查看以下链接：

- [使用 Elm 生成随机数官方文档（英文）](https://guide.elm-lang.org/architecture/random.html)
- [Elm 文档中 Random 包的详细信息（英文）](https://package.elm-lang.org/packages/elm/random/latest/)

感谢阅读本文，希望您可以顺利地在 Elm 中生成随机数。谢谢！
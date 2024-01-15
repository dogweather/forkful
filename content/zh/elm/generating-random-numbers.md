---
title:                "生成随机数"
html_title:           "Elm: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么

在编写程序时，我们经常需要使用随机数来做一些决策或者产生一些随机的结果。Elm语言提供了简单易用的工具来生成随机数，让我们来看看如何操作吧！

## 如何操作

要生成随机数，我们需要使用`Random`模块。首先，让我们导入该模块：

```elm
import Random
```

接下来，我们可以使用`generate`函数来生成一个可用于随机数的生成器：

```elm
Random.generate getRandomNumber
```

这里，`getRandomNumber`是我们自定义的一个函数，它将会接收一个随机数作为参数。我们可以在函数体中定义一些对随机数的操作，例如将其打印出来：

```elm
getRandomNumber : Int -> Cmd msg
getRandomNumber randomNumber =
    Cmd.log randomNumber
```

我们也可以定义一个范围来限定随机数的值：

```elm
Random.generate (Random.int 1 10) --> 生成1到10之间的随机整数
Random.generate (Random.float 0 1) --> 生成0到1之间的随机小数
```

除了整数和小数，我们也可以生成布尔值和字符等其他类型的随机数。具体操作可以参考Elm官方文档中的`Random`模块。

## 深入了解

在生成随机数时，我们可以通过设置种子来控制生成的随机数序列。这在测试中非常有用，因为我们可以固定生成器的种子来确保每次测试都得到相同的随机数。我们可以使用`initialSeed`函数来创建一个指定种子的生成器：

```elm
Random.initialSeed 1234 --> 以种子1234创建一个生成器
```

Elm还提供了`Seed`类型来表示种子，我们可以使用`Generator`类型来创建自定义的随机数生成器。这些更深层次的操作可以进一步提高我们使用随机数的灵活性。

## 参考文档

- [Elm官方文档 - Random模块](https://package.elm-lang.org/packages/elm/core/latest/Random)
- [Elm官方文档 - Seed类型](https://package.elm-lang.org/packages/elm/core/latest/Random#Seed)
- [Elm官方文档 - Generator类型](https://package.elm-lang.org/packages/elm/core/latest/Random#Generator)

## 相关阅读

- [Elm语言官网](https://elm-lang.org/)
- [Elm中文网](https://elm-china.org/)
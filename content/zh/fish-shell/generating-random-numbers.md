---
title:                "Fish Shell: 产生随机数"
programming_language: "Fish Shell"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么

生成随机数是编程中常见的需求，例如创建随机密码或选择随机样本。使用 Fish Shell 可以轻松地实现这一功能。

## 怎么做

使用 Fish Shell 内置的 `math` 模块就可以生成随机数。以下是一个例子，使用 `math` 模块中的 `random` 函数来生成一个 1-10 之间的随机数：

```Fish Shell
set random_number (math random --uniform 1 10)
echo $random_number
```

运行这段代码，你会看到一个随机数被打印出来。每次运行代码，都会生成一个不同的随机数。

## 深入探讨

Fish Shell 的 `math` 模块内置了多种随机数生成函数，包括 `random`、`normal`、`poisson` 等。你可以使用不同的函数来生成不同分布的随机数，满足不同的需求。此外，你也可以通过设置参数来控制生成随机数的范围。通过学习 `math` 模块的文档，你可以更深入地了解如何使用 Fish Shell 生成随机数。

## 参考文献

- [Fish Shell 官方文档](https://fishshell.com/docs/current/index.html)
- [熟悉 Fish Shell 中的 Random 模块](https://fishshell.com/docs/current/cmds/math.html#math-random)
- [学习 Fish Shell 的数学函数](https://fishshell.com/docs/current/cmds/math.html)

## 另请参阅

- [在 Fish Shell 中学习循环](https://fishshell.com/docs/current/tutorial.html#loop-de-loops)
- [使用 Fish Shell 进行系统管理](https://fishshell.com/blog/2016/10/14/commands-part-4.html)
---
title:                "Gleam: 生成随机数"
programming_language: "Gleam"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 为什么

在编程世界中，生成随机数是一项非常重要的技能。无论是用于游戏、加密还是其他应用程序，生成随机数都是必不可少的。使用Gleam编程语言，我们可以轻松地生成高质量的随机数，让我们来看看如何做到这一点。

## 如何做到

要使用Gleam生成随机数，我们需要使用标准库中的Random模块。首先，我们需要在代码的开头导入Random模块：

```
import gleam/random
```

接下来，我们可以使用Random模块中的函数来生成随机数，例如，我们可以使用`int`函数来生成一个随机的整数：

```
let random_int = random.int(1, 100)
```

以上代码将会生成一个1到100之间的随机整数，并将其赋值给`random_int`变量。

我们也可以使用`float`函数来生成随机的浮点数：

```
let random_float = random.float(0.0, 1.0)
```

以上代码将会生成一个0到1之间的随机浮点数，并将其赋值给`random_float`变量。

Gleam还提供了其他几个函数来生成不同类型的随机数，例如`bool`用于生成随机的布尔值，`string`用于生成随机的字符串等等。现在，您已经知道如何在Gleam中生成随机数，让我们深入了解一下。

## 深入了解

Gleam的Random模块使用Mersenne Twister算法来生成随机数，这是一种广泛使用的随机数生成方法，它可以生成高质量的随机数序列。此外，Gleam还提供了伪随机数生成功能，这意味着每次执行程序都会生成相同的随机数序列，这在调试代码时非常方便。

需要注意的是，在使用随机数时，我们需要遵循一些安全措施来防止被攻击。例如，在密码学应用中，我们应该使用密码学安全的随机数生成方法，而不是普通的随机数生成方法。详细的安全措施可以参考Gleam官方文档。

## 参考资料

- Gleam官方文档：https://gleam.run/documentation/standard-libraries
- Mersenne Twister算法：https://en.wikipedia.org/wiki/Mersenne_Twister
- 密码学安全的随机数生成方法：https://www.chosenplaintext.ca/articles/secure-random-in-crypto.html

# 参见
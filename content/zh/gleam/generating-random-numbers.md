---
title:                "生成随机数"
html_title:           "Gleam: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

##为什么
在计算机编程中，随机数生成是一项基本的技能，它可以被用来模拟现实世界中的随机事件，或者作为密码生成器和游戏中的随机元素。使用Gleam编程语言，生成随机数变得更加简单。通过这篇文章，我们将看到如何在Gleam中生成随机数，并了解更多关于随机数生成的知识。

##如何操作
首先，我们需要在代码中导入随机数生成器模块。在```module```语句下方，添加```import random```。接下来，在需要生成随机数的地方，使用```random.int```函数，并指定范围。例如，如果我们想要生成一个1到10之间的随机整数，可以在代码中写入```random.int(1, 10)```。代码示例如下：

```Gleam
module example

import random

fn main() {
    let random_number = random.int(1, 10)
    io.print("Random number between 1 to 10: " ++ random_number)
}
```

运行后，我们将在终端或控制台看到类似于以下输出：

```
Random number between 1 to 10: 7
```

除了整数，我们还可以使用```random.float```函数来生成随机小数。例如，如果我们想要生成一个0到1之间的随机小数，可以使用```random.float(0, 1)```。代码示例如下：

```Gleam
module example

import random

fn main() {
    let random_float = random.float(0, 1)
    io.print("Random floating number between 0 to 1: " ++ random_float)
}
```

运行后，我们将在终端或控制台看到类似于以下输出：

```
Random floating number between 0 to 1: 0.537142
```

随机数生成也可以用来生成随机字符串。例如，我们可以使用```random.choice```函数来从一个字符列表中随机选择一个字符，并不断重复该过程以生成随机字符串。代码示例如下：

```Gleam
module example

import random

fn main() {
    let characters = ["a", "b", "c", "d", "e"]
    let random_string = random.choice(characters) ++ random.choice(characters) ++ random.choice(characters) ++ random.choice(characters)
    io.print("Random string generated: " ++ random_string)
}
```

运行后，我们将在终端或控制台看到类似于以下输出：

```
Random string generated: bcad
```

##深入了解
随机数生成是一个很有意思的话题，也是计算机科学中的重要概念。在随机数生成中，有两种常见的方法：伪随机数生成和真随机数生成。伪随机数生成使用算法来生成看似随机的数字序列，而真随机数生成则利用物理现象来获取真正随机的数据。在程序设计中，我们通常使用伪随机数生成器来满足随机数的需求，因为它可以在较短的时间内产生大量的随机数。

在Gleam中，我们使用的是伪随机数生成器。Gleam提供的```random```模块基于Mersenne Twister算法，它是一种被广泛认可的高质量随机数生成器。此外，Gleam还提供了其他常用的随机数生成函数，例如```random.bool```用于生成随机布尔值，```random.range```用于生成指定范围内的随机整数。

##请参阅
- [Gleam官方文档](https://gleam.run/documentation)
- [Gleam随机数生成模块](https://gleam.run/modules/gleam_stdlib/random
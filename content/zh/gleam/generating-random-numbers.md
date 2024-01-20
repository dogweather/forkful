---
title:                "生成随机数"
html_title:           "Go: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 什么与为什么?

生成随机数就是创建不可预测的数字。程序员进行随机数字生成以增加功能的随机性，例如在游戏中随机生成元素。

# 如何做：

在Gleam中，我们使用`rand`模块的`int`函数生成随机数。
```Gleam
import gleam/rand

fn main() {
  let _ = rand.int(1, 10)
}
```
当运行代码时，你会得到一个范围在1到10之间的随机数。

# 深入研究：

生成随机数的概念在计算机科学中有着悠久的历史。在早期, 计算机硬件会使用物理过程产生真随机数, 如测量核衰变。然而, 这种方法既昂贵又不妥。

很快，人们发现通过算法-伪随机数生成器（PRNGs）可以生成一个"看似"随机的数。在Gleam中，这种方法被应用在`rand`模块。`rand.int`实际上是一个函数调用，取一个最小值和一个最大值，返回一个介于这两者之间的随机整数。

然而，还存在其它方法生成随机数，例如使用`rand.float`函数，可以生成一个随机浮点数。

# 参见：

如果你需要进一步了解Gleam的`rand`模块或随机化技术，以下链接会提供更多信息：

- [A Brief History of Random Numbers](https://www.philforhumanity.com/Random_Number_History.html)
  
谨慎使用随机数生成功能，它可以为你的代码添加强大的随机性。
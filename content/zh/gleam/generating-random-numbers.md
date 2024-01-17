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

## 什么是生成随机数? 
生成随机数意味着使用计算机来生成随机的数字或值。程序员经常会使用这种技术来增加程序的随机性，例如在游戏中生成随机的地图，或者在测试程序时生成随机的数据集。 

## 如何实现:
以下是一个在Gleam中生成随机数的例子：
```Gleam
import rand

fn main() {
  let random_num = rand.int(100)
  io.println("生成的随机数是: {{random_num}}")
}
```
输出示例：生成的随机数是: 57

## 深入了解:
生成随机数在计算机编程中非常常见，并且有许多不同的方法来实现它。在Gleam中，我们使用rand模块来生成随机数，但是其他编程语言也有类似的方法。另外，有一些算法可以计算出更真正的随机数，例如通过测量自然现象来获取真正的随机数。

## 链接:
了解更多关于Gleam中生成随机数的信息，请访问官方文档：https://gleam.run/libraries/rand.html
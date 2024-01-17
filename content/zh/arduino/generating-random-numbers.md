---
title:                "生成随机数"
html_title:           "Arduino: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么和为什么？
生成随机数是指通过计算机程序产生一系列不可预测的数字。程序员这样做的原因是为了增加程序的随机性，使其更加灵活和有趣。

## 如何操作：
下面是一个简单的例子，展示如何通过Arduino生成随机数：

```arduino
// 引入随机数库
#include <stdlib.h>

// 设置随机种子
randomSeed(analogRead(A0));

// 生成5个随机数
for (int i = 0; i < 5; i++) {
  int randomNumber = random(10);
  Serial.println(randomNumber);
}

// 示例输出：
// 5
// 2
// 8
// 3
// 0
```

## 深入了解：
生成随机数的历史可以追溯到20世纪50年代，当时的计算机程序员发现需要增加随机因素来提高计算机游戏的趣味性。除了使用Arduino内置的随机函数，还可以使用外部模块或传感器来提高随机性。但是，尽管随机数看起来是随意的，但实际上它们是根据特定的算法来生成的。

## 参考链接：
- [Arduino官方文档 - random()](https://www.arduino.cc/reference/zh/language/functions/random-numbers/random/)
- [哈佛大学课程：随机和模拟 - 随机函数的历史](http://etutorials.org/Misc/computer+science+theory/Part+2+A+Random+Processes+Hidden+Structure/Chapter+13+Randomness+in+Computer+Science/13.2+Random+Algorithms/SOME+CLASSIC+RANDOM+ALGORITHMS/)
- [解决随机数不随机的问题的方法](https://www.csoonline.com/article/3212878/what-you-need-to-know-about-random-number-generators.html)
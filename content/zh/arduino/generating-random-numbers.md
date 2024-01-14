---
title:                "Arduino: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 为何要使用Arduino生成随机数

在编写Arduino程序时，我们经常会遇到需要生成随机数的情况。随机数可以帮助我们创建更多样化和有趣的程序，增加程序的互动性，让我们的项目更有趣。因此，学习如何使用Arduino生成随机数是非常有用的。

## 如何使用Arduino生成随机数

在Arduino编程中，我们可以使用`random()`函数来生成随机数。这个函数能够产生从0到指定范围内的随机数，我们只需要在括号内输入范围的最小值和最大值即可。例如，如果我们想要产生从1到10之间的随机数，我们可以这样写：

```Arduino
int randomNumber = random(1, 11); // 定义一个变量来存储随机数
Serial.println(randomNumber); // 打印随机数到串口监视器
```

这样，每次我们运行程序，都会得到一个1到10之间的随机数。

## 深入了解随机数生成

在电子设备中，实现真正的随机数是一件非常困难的事情。因此，Arduino使用了伪随机数生成器（PRNG）来产生随机数。PRNG其实是一个算法，它通过一个种子（seed）值来计算下一个随机数，而种子值是我们所指定的范围。因此，当我们每次运行程序时，都会得到相同的随机数序列。

为了让随机数看起来更加随机，我们可以使用一个不断变化的种子值。Arduino中的`randomSeed()`函数可以实现这个功能。它通常使用`analogRead()`函数读取模拟引脚上的电压值来作为种子值，因为模拟引脚上的电压值每次都会变化。

## 参考阅读

- Arduino官方文档：https://www.arduino.cc/reference/en/language/functions/random-numbers/random/
- PRNG知识介绍：https://en.wikipedia.org/wiki/Pseudorandom_number_generator
- `analogRead()`函数介绍：https://www.arduino.cc/reference/en/language/functions/analog-io/analogread/
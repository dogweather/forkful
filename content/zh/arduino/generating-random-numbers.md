---
title:                "Arduino: 生成随机数"
programming_language: "Arduino"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 为什么要用Arduino编程生成随机数？

随机数在编程中有多种应用，比如用来产生游戏中的随机事件或者给传感器添加噪声，使得数据更加真实。通过使用Arduino编程来生成随机数，可以让你的项目更加有趣和实用。

# 如何在Arduino编程中生成随机数？

在Arduino编程中，我们可以使用内置的随机数函数`random()`来生成随机数。代码示例如下：

```
Arduino
int randomNumber = random(1, 10); //生成1到10的随机数
Serial.println(randomNumber); //将随机数打印到串口监视器
```

运行上述代码，你会发现每次生成的随机数都不同。如果你想每次运行程序时产生不同的随机数序列，可以在`setup()`函数中加上`randomSeed()`函数，将一个可变的值作为种子。

# 深入了解随机数生成

随机数并非完全随机，它们是由一个称为“伪随机数发生器”的算法生成的。这个算法通过某种计算方式来产生看似随机的数列。每个算法都有其特定的种子，通过改变种子可以产生不同的随机数序列。

在使用随机数时，我们需要注意种子的选择，因为相同的种子会产生相同的随机数序列。此外，随机数并不是真正的随机，它只能产生有限的数列，如果你想要更随机的数列，可以尝试引入其他的数据或者改变种子值。

# 参考链接

- [Arduino随机数函数文档](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [伪随机数发生器](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
- [如何在Arduino中使用随机数](https://www.makerguides.com/random-numbers-arduino/)
- [使用随机数的5个实例](https://www.jameco.com/jameco/workshop/howitworks/5-applications-for-random-number-generators-arduino.html)

## 参见

- [Arduino编程基础知识](https://www.arduino.cc/)
- [使用Arduino编程控制传感器](https://www.wikihow.com/Control-a-Sensor-With-Arduino)
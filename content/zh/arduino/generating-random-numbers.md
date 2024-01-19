---
title:                "生成随机数"
html_title:           "Go: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么和为什么?

在编程中，生成随机数是为了创建不可预测的结果。程序员经常使用它们以增加复杂性，例如在游戏设计或加密中。

## 如何做到：

Arduino 提供了一个简单的 `random()` 函数来生成随机数。您只需要为其提供最小值和最大值范围，它就会返回该范围内的随机数。
例子如下：

```Arduino
void setup() {
  Serial.begin(9600); // 初始化串口通信
}

void loop() {
  int randNumber = random(10, 100); // 生成10到100之间的随机数
  Serial.println(randNumber); // 将生成的随机数输出到串口控制台
  delay(1000); // 延迟1秒
}
```

以上代码的输出将会是10到100间的一个随机整数，每秒生成一个。

## 深度研究：

虽然 Arduino 的 `random()` 函数看起来简单易用，但背后还有一些历史和实现细节值得我们了解的。

1. 历史背景：随机数生成是计算机科学领域早期就正在探究的问题，早在20世纪50年代，人们就已开始设计硬件随机数生成器，随着时间的推移，出现了众多算法和实现方式。

2. 替代方法：Arduino 还有其他方式可以生成随机数，例如 `rand()` 函数，它和 `random()` 不同的是，它会返回一个完全随机的数，取决于您怎样种子 `srand()`。

3. 实现细节：尽管我们通常说 `random()` 生成了一个"随机数"，实际上它是一个"伪随机数"。这是因为它基于一个固定的算法生成数字，所以如果你多次运行相同的代码，你将会得到同样的结果序列。为了真正的随机，您可以使用 `randomSeed()` 函数，用当前时间作为种子，它可以改变生成的数字序列，使得更接近真正的随机。

## 另请参阅：

1. Arduino 官方文档: [random()](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/) 函数和 [randomSeed()](https://www.arduino.cc/reference/en/language/functions/random-numbers/randomseed/) 函数。

2. [每日一编：如何在 Arduino 上生成伪随机数](https://chinese.makezine.com/2013/04/02/daily-arduino-tip-how-to-generate-pseudo-random-numbers-on-an-arduino/)

3. Seifer Tim的博文: [真随机还是伪随机？计算机如何生成随机数](https://medium.com/@timseifer/random-or-pseudo-random-how-computers-generate-random-numbers-74971cd6f131)
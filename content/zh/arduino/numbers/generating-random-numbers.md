---
title:                "生成随机数"
aliases: - /zh/arduino/generating-random-numbers.md
date:                  2024-01-27T20:32:42.906945-07:00
model:                 gpt-4-0125-preview
simple_title:         "生成随机数"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么和为什么？
在Arduino项目中生成随机数涉及到设计时就不可预测的数值生产，对于游戏、模拟和安全系统等应用至关重要。程序员利用这项技术引入变量或做出不应该是确定性的决策。

## 如何操作：
Arduino提供了生成随机数的直接函数：`randomSeed()`和`random()`。首先，为了确保每次程序运行时数字序列的不同，需要初始化随机数生成器的种子。一个常用的方法是通过从未连接的引脚进行模拟读取来种子。

```Arduino
void setup() {
  Serial.begin(9600);
  // 初始化随机种子
  randomSeed(analogRead(0));
}

void loop() {
  // 生成一个0到99之间的随机数
  int randomNumber = random(100);
  Serial.println(randomNumber);
  delay(1000); // 延迟一秒以便输出的可读性
}
```

上面的程序在`setup()`函数中初始化了随机数生成器，并在每次循环迭代中生成一个介于0到99之间的新数，将该数输出到串行监视器。

示例输出：
```
42
17
93
...
```

## 深入了解
Arduino的`random()`函数在底层利用了一个伪随机数生成器（PRNG），它遵循一个确定性序列但看起来统计随机。序列的初始值或种子，极大地影响了其不可预测性，因此常用`randomSeed()`配合某种程度上随机的输入作为起点。值得注意的是，Arduino生成的随机性对于大多数业余项目来说已经足够，但由于其随时间的可预测性，可能不满足高安全性应用的标准。对于加密目的，建议研究更复杂的算法和硬件随机数生成器（HRNGs），这些可以通过利用物理过程提供真正的随机性。

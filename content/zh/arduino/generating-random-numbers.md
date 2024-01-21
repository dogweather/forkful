---
title:                "生成随机数"
date:                  2024-01-20T17:48:34.310969-07:00
model:                 gpt-4-1106-preview
simple_title:         "生成随机数"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
生成随机数，就是让Arduino创造出不可预测的数字，用来模拟抽奖或实现测试数据。程序员用它们来加强安全性、增加游戏的不确定性或进行科学模拟。

## How to:
```Arduino
void setup() {
  Serial.begin(9600);
  randomSeed(analogRead(0)); // 初始化随机数种子
}

void loop() {
  int randomNumber = random(1, 100); // 生成1到99的随机数
  Serial.println(randomNumber);
  delay(1000); // 每秒生成一次
}
```
示例输出：
```
23
58
79
...
```

## Deep Dive
Arduino生成随机数背后的历史和技术结构很有趣。使用`randomSeed()`来初始化随机数种子是至关重要的，通常利用读取未连接的模拟输入口的值来增加随机性。如果不设置种子，Arduino每次重启都会产生同一系列的“随机”数字。

替代方案包括使用外部硬件、温度传感器读数或者声音强度来生成随机种子。重要的是，Arduino的随机数生成器是基于一个确定性算法的伪随机数生成器，并不能用于需要高安全性的应用。

详细了解随机数如何实现，看看[`random()`](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)函数和[随机数种子](https://www.arduino.cc/reference/en/language/functions/random-numbers/randomseed/)官方文档。

## See Also

- 官方[`random()`函数说明](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- 官方[`randomSeed()`说明](https://www.arduino.cc/reference/en/language/functions/random-numbers/randomseed/)
- [伪随机数生成器 (PRNGs)](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)维基百科页面
---
title:                "处理复数"
date:                  2024-01-26T04:36:52.916098-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理复数"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## 什么和为什么？
复数有实部和虚部，通常写作`a + bi`。它们对于一些涉及信号处理、电气工程或任何其他最好在平面中建模的现象的数学密集型Arduino项目至关重要。

## 如何操作：
```Arduino
#include <Complex.h>

void setup() {
  Serial.begin(9600); // 开始串行通信
  
  Complex myComplex(2, 3); // 创建一个复数 2 + 3i
  Complex anotherComplex(1, 1); // 创建另一个复数 1 + 1i
  
  // 加法
  Complex result = myComplex + anotherComplex; 
  Serial.print("加法: "); 
  result.print(); // 输出 3 + 4i
  
  // 乘法
  result = myComplex * anotherComplex; 
  Serial.print("乘法: ");
  result.print(); // 输出 -1 + 5i
}

void loop() {
  // 在这个例子中未使用
}
```
示例输出：
```
加法: 3 + 4i
乘法: -1 + 5i
```

## 深入探讨
最初，复数遭到怀疑，但它们已经在各个科学领域占据了中心地位。从历史上看，它们因提供了缺乏实数解的多项式方程的解决方案而被认可。

Arduino标准库中不包含复数，但您可以利用如`Complex.h`这样的库来处理它们。这些库内部通常通过使用两个双精度浮点数来存储实部和虚部，并重载运算符以支持算术运算来定义Complex类。

作为一种替代方案，对于那些本质上不需要复数算术的应用，考虑使用其他数学策略或库。但请记住，使用浮点数代替复数可能会过于简化一些问题。

## 另请参阅
- Rob Tillaart编写的[Complex.h](https://github.com/RobTillaart/Complex)库。
- 深入探究[复数背后的数学知识](https://mathworld.wolfram.com/ComplexNumber.html)。

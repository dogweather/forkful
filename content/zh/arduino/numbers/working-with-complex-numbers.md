---
date: 2024-01-26 04:36:52.916098-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u6700\u521D\uFF0C\u590D\u6570\u906D\u5230\
  \u6000\u7591\uFF0C\u4F46\u5B83\u4EEC\u5DF2\u7ECF\u5728\u5404\u4E2A\u79D1\u5B66\u9886\
  \u57DF\u5360\u636E\u4E86\u4E2D\u5FC3\u5730\u4F4D\u3002\u4ECE\u5386\u53F2\u4E0A\u770B\
  \uFF0C\u5B83\u4EEC\u56E0\u63D0\u4F9B\u4E86\u7F3A\u4E4F\u5B9E\u6570\u89E3\u7684\u591A\
  \u9879\u5F0F\u65B9\u7A0B\u7684\u89E3\u51B3\u65B9\u6848\u800C\u88AB\u8BA4\u53EF\u3002\
  \u2026"
lastmod: '2024-04-05T21:53:48.351161-06:00'
model: gpt-4-0125-preview
summary: "Arduino\u6807\u51C6\u5E93\u4E2D\u4E0D\u5305\u542B\u590D\u6570\uFF0C\u4F46\
  \u60A8\u53EF\u4EE5\u5229\u7528\u5982`Complex.h`\u8FD9\u6837\u7684\u5E93\u6765\u5904\
  \u7406\u5B83\u4EEC\u3002\u8FD9\u4E9B\u5E93\u5185\u90E8\u901A\u5E38\u901A\u8FC7\u4F7F\
  \u7528\u4E24\u4E2A\u53CC\u7CBE\u5EA6\u6D6E\u70B9\u6570\u6765\u5B58\u50A8\u5B9E\u90E8\
  \u548C\u865A\u90E8\uFF0C\u5E76\u91CD\u8F7D\u8FD0\u7B97\u7B26\u4EE5\u652F\u6301\u7B97\
  \u672F\u8FD0\u7B97\u6765\u5B9A\u4E49Complex\u7C7B."
title: "\u5904\u7406\u590D\u6570"
weight: 14
---

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

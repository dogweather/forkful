---
date: 2024-01-27 20:32:42.906945-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Arduino\u63D0\u4F9B\u4E86\u751F\u6210\
  \u968F\u673A\u6570\u7684\u76F4\u63A5\u51FD\u6570\uFF1A`randomSeed()`\u548C`random()`\u3002\
  \u9996\u5148\uFF0C\u4E3A\u4E86\u786E\u4FDD\u6BCF\u6B21\u7A0B\u5E8F\u8FD0\u884C\u65F6\
  \u6570\u5B57\u5E8F\u5217\u7684\u4E0D\u540C\uFF0C\u9700\u8981\u521D\u59CB\u5316\u968F\
  \u673A\u6570\u751F\u6210\u5668\u7684\u79CD\u5B50\u3002\u4E00\u4E2A\u5E38\u7528\u7684\
  \u65B9\u6CD5\u662F\u901A\u8FC7\u4ECE\u672A\u8FDE\u63A5\u7684\u5F15\u811A\u8FDB\u884C\
  \u6A21\u62DF\u8BFB\u53D6\u6765\u79CD\u5B50\u3002"
lastmod: '2024-04-05T21:53:48.353639-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u751F\u6210\u968F\u673A\u6570"
weight: 12
---

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

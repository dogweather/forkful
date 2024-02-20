---
date: 2024-01-27 20:32:42.906945-07:00
description: "\u5728Arduino\u9879\u76EE\u4E2D\u751F\u6210\u968F\u673A\u6570\u6D89\u53CA\
  \u5230\u8BBE\u8BA1\u65F6\u5C31\u4E0D\u53EF\u9884\u6D4B\u7684\u6570\u503C\u751F\u4EA7\
  \uFF0C\u5BF9\u4E8E\u6E38\u620F\u3001\u6A21\u62DF\u548C\u5B89\u5168\u7CFB\u7EDF\u7B49\
  \u5E94\u7528\u81F3\u5173\u91CD\u8981\u3002\u7A0B\u5E8F\u5458\u5229\u7528\u8FD9\u9879\
  \u6280\u672F\u5F15\u5165\u53D8\u91CF\u6216\u505A\u51FA\u4E0D\u5E94\u8BE5\u662F\u786E\
  \u5B9A\u6027\u7684\u51B3\u7B56\u3002"
lastmod: 2024-02-19 22:05:07.111759
model: gpt-4-0125-preview
summary: "\u5728Arduino\u9879\u76EE\u4E2D\u751F\u6210\u968F\u673A\u6570\u6D89\u53CA\
  \u5230\u8BBE\u8BA1\u65F6\u5C31\u4E0D\u53EF\u9884\u6D4B\u7684\u6570\u503C\u751F\u4EA7\
  \uFF0C\u5BF9\u4E8E\u6E38\u620F\u3001\u6A21\u62DF\u548C\u5B89\u5168\u7CFB\u7EDF\u7B49\
  \u5E94\u7528\u81F3\u5173\u91CD\u8981\u3002\u7A0B\u5E8F\u5458\u5229\u7528\u8FD9\u9879\
  \u6280\u672F\u5F15\u5165\u53D8\u91CF\u6216\u505A\u51FA\u4E0D\u5E94\u8BE5\u662F\u786E\
  \u5B9A\u6027\u7684\u51B3\u7B56\u3002"
title: "\u751F\u6210\u968F\u673A\u6570"
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

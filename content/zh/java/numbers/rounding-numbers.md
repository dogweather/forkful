---
date: 2024-01-26 03:45:03.926876-07:00
description: "\u5982\u4F55\u8FDB\u884C\uFF1A Java\u63D0\u4F9B\u4E86\u591A\u79CD\u6570\
  \u5B57\u820D\u5165\u7684\u65B9\u5F0F\u3002\u8FD9\u91CC\u6709\u4E2A\u4F7F\u7528`Math.round()`\u3001\
  `BigDecimal`\u548C`DecimalFormat`\u7684\u5FEB\u901F\u793A\u4F8B\u3002"
lastmod: '2024-03-13T22:44:47.619986-06:00'
model: gpt-4-0125-preview
summary: "Java\u63D0\u4F9B\u4E86\u591A\u79CD\u6570\u5B57\u820D\u5165\u7684\u65B9\u5F0F\
  \u3002\u8FD9\u91CC\u6709\u4E2A\u4F7F\u7528`Math.round()`\u3001`BigDecimal`\u548C\
  `DecimalFormat`\u7684\u5FEB\u901F\u793A\u4F8B."
title: "\u6570\u5B57\u53D6\u6574"
weight: 13
---

## 如何进行：
Java提供了多种数字舍入的方式。这里有个使用`Math.round()`、`BigDecimal`和`DecimalFormat`的快速示例。

```java
public class RoundingDemo {
    public static void main(String[] args) {
        double num = 123.4567;

        // 使用 Math.round()
        long roundedNum = Math.round(num);
        System.out.println(roundedNum); // 输出：123

        // 使用 BigDecimal 以获得更多控制权
        BigDecimal bd = new BigDecimal(num).setScale(2, RoundingMode.HALF_UP);
        double roundedBigDecimal = bd.doubleValue();
        System.out.println(roundedBigDecimal); // 输出：123.46

        // 使用 DecimalFormat
        DecimalFormat df = new DecimalFormat("#.##");
        String formattedNum = df.format(num);
        System.out.println(formattedNum); // 输出：123.46
    }
}
```

## 深入探究
从历史上看，舍入数字对于模拟计算至关重要，并且已经延续到数字计算中，以提高效率和精度。如浮点算术中的舍入误差所示，这不是一个微不足道的问题——它们可以累积地破坏航空航天和金融应用中的计算。

除了`Math.round()`，你还可以使用`BigDecimal`，它允许你更细致地控制比例和舍入模式，并使用`DecimalFormat`，当你需要在格式化文本输出时舍入数字。舍入的替代方法包括向下舍入、向上进位和截断，这些都是处理精度的不同方式，通常由各种`Math`方法处理。

根据你的使用场景，舍入策略可能会有所不同。比如，对于金融计算，`BigDecimal`是首选，因为在这种场景下，精准度至关重要。相比之下，`Math.round()`是进行通用操作的快捷方式，当你对舍入模式不那么挑剔时。

## 另见
- [Oracle的Java数学文档](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/Math.html)
- [浮点算术的IEEE标准（IEEE 754）](https://ieeexplore.ieee.org/document/4610935)
- [Java中的DecimalFormat类](https://docs.oracle.com/javase/7/docs/api/java/text/DecimalFormat.html)

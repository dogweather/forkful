---
date: 2024-01-26 03:45:03.926876-07:00
description: "\u820D\u5165\u6570\u5B57\u610F\u5473\u7740\u5C06\u6570\u5B57\u8C03\u6574\
  \u81F3\u6307\u5B9A\u7684\u7CBE\u5EA6\u7B49\u7EA7\u3002\u7A0B\u5E8F\u5458\u8FDB\u884C\
  \u820D\u5165\u64CD\u4F5C\u662F\u4E3A\u4E86\u7B80\u5316\u6570\u5B57\u4EE5\u63D0\u9AD8\
  \u53EF\u8BFB\u6027\uFF0C\u6EE1\u8DB3\u7279\u5B9A\u89C4\u8303\uFF0C\u6216\u786E\u4FDD\
  \u8BA1\u7B97\u5728\u67D0\u4E9B\u8FB9\u754C\u5185\uFF0C\u6BD4\u5982\u907F\u514D\u6D6E\
  \u70B9\u7B97\u672F\u4E2D\u7684\u7CBE\u5EA6\u9519\u8BEF\u3002"
lastmod: '2024-03-13T22:44:47.619986-06:00'
model: gpt-4-0125-preview
summary: "\u820D\u5165\u6570\u5B57\u610F\u5473\u7740\u5C06\u6570\u5B57\u8C03\u6574\
  \u81F3\u6307\u5B9A\u7684\u7CBE\u5EA6\u7B49\u7EA7\u3002\u7A0B\u5E8F\u5458\u8FDB\u884C\
  \u820D\u5165\u64CD\u4F5C\u662F\u4E3A\u4E86\u7B80\u5316\u6570\u5B57\u4EE5\u63D0\u9AD8\
  \u53EF\u8BFB\u6027\uFF0C\u6EE1\u8DB3\u7279\u5B9A\u89C4\u8303\uFF0C\u6216\u786E\u4FDD\
  \u8BA1\u7B97\u5728\u67D0\u4E9B\u8FB9\u754C\u5185\uFF0C\u6BD4\u5982\u907F\u514D\u6D6E\
  \u70B9\u7B97\u672F\u4E2D\u7684\u7CBE\u5EA6\u9519\u8BEF\u3002"
title: "\u6570\u5B57\u53D6\u6574"
---

{{< edit_this_page >}}

## 什么是舍入以及为什么要舍入？
舍入数字意味着将数字调整至指定的精度等级。程序员进行舍入操作是为了简化数字以提高可读性，满足特定规范，或确保计算在某些边界内，比如避免浮点算术中的精度错误。

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

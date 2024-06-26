---
date: 2024-01-26 04:43:49.640006-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Java \u6CA1\u6709\u5185\u7F6E\u5BF9\u590D\
  \u6570\u7684\u652F\u6301\uFF0C\u4F46\u6211\u4EEC\u53EF\u4EE5\u81EA\u5DF1\u7F16\u5199\
  \u4E00\u4E2A\u7C7B\u6216\u4F7F\u7528\u5E93\u3002\u8FD9\u91CC\u662F\u4E00\u4E2A\u5982\
  \u4F55\u521B\u5EFA\u4E00\u4E2A\u7B80\u5355\u7684 `ComplexNumber` \u7C7B\u5E76\u4F7F\
  \u7528\u5B83\u7684\u5FEB\u901F\u793A\u4F8B\uFF1A."
lastmod: '2024-04-05T22:38:46.780228-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A Java \u6CA1\u6709\u5185\u7F6E\u5BF9\u590D\
  \u6570\u7684\u652F\u6301\uFF0C\u4F46\u6211\u4EEC\u53EF\u4EE5\u81EA\u5DF1\u7F16\u5199\
  \u4E00\u4E2A\u7C7B\u6216\u4F7F\u7528\u5E93\u3002\u8FD9\u91CC\u662F\u4E00\u4E2A\u5982\
  \u4F55\u521B\u5EFA\u4E00\u4E2A\u7B80\u5355\u7684 `ComplexNumber` \u7C7B\u5E76\u4F7F\
  \u7528\u5B83\u7684\u5FEB\u901F\u793A\u4F8B\uFF1A."
title: "\u5904\u7406\u590D\u6570"
weight: 14
---

## 如何操作：
Java 没有内置对复数的支持，但我们可以自己编写一个类或使用库。这里是一个如何创建一个简单的 `ComplexNumber` 类并使用它的快速示例：

```java
public class ComplexNumber {
    private double real;
    private double imaginary;

    public ComplexNumber(double real, double imaginary) {
        this.real = real;
        this.imaginary = imaginary;
    }

    public ComplexNumber add(ComplexNumber other) {
        return new ComplexNumber(this.real + other.real, this.imaginary + other.imaginary);
    }

    // ToString 用于以 a + bi 的形式显示复数
    @Override
    public String toString() {
        return String.format("%.1f + %.1fi", real, imaginary);
    }

    // 快速测试
    public static void main(String[] args) {
        ComplexNumber c1 = new ComplexNumber(2, 3);
        ComplexNumber c2 = new ComplexNumber(1, 4);

        System.out.println("和: " + c1.add(c2));
    }
}
```

主方法的示例输出将是：

```
和: 3.0 + 7.0i
```

## 深入了解
在 Java 这样的高级语言出现之前，程序员直接使用 Fortran 或 C 语言中的数学库来处理复杂操作。这个概念可以追溯到 16 世纪，归功于如 Gerolamo Cardano 和 Rafael Bombelli 这样的数学家。

在 Java 中，`java.lang.Math` 对基本需要是首选，但它跳过了复数，可能是因为并非每个程序员都会用到它们。替代方案？使用库。Apache Commons Math 提供了一个包含各种操作方法的 `Complex` 类。不过，自己实现的好处也很明显：轻量级，完全根据你的实际需求量身定制，且无需库的开销。

一个重要的细节：注意浮点数精度。计算机无法精确表示一些数字，导致舍入误差。在执行重复的复杂操作时，这些误差可能会累积！

## 另见
深入研究和更复杂的操作，请查阅：

- [Apache Commons Math](https://commons.apache.org/proper/commons-math/)
- [JScience 的 Complex 类](http://jscience.org/)
- Oracle 关于[浮点算术](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)的教程

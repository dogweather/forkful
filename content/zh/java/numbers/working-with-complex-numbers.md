---
date: 2024-01-26 04:43:49.640006-07:00
description: "\u901A\u8FC7\u589E\u52A0\u4E00\u4E2A\u865A\u6570\u5355\u4F4D `i`\uFF08\
  \u5176\u4E2D `i^2 = -1`\uFF09\uFF0C\u590D\u6570\u6269\u5C55\u4E86\u5B9E\u6570\u7EBF\
  \u3002\u5728\u5DE5\u7A0B\u5B66\u3001\u7269\u7406\u5B66\u4EE5\u53CA\u9AD8\u7EA7\u6570\
  \u5B66\u7B49\u9886\u57DF\uFF0C\u590D\u6570\u81F3\u5173\u91CD\u8981\uFF0C\u56E0\u4E3A\
  \u5B83\u4EEC\u80FD\u591F\u6A21\u62DF\u5B9E\u6570\u65E0\u6CD5\u5904\u7406\u7684\u73B0\
  \u8C61\uFF0C\u5982\u7535\u6D41\u548C\u4FE1\u53F7\u5904\u7406\u3002"
lastmod: '2024-03-13T22:44:47.618898-06:00'
model: gpt-4-0125-preview
summary: "\u901A\u8FC7\u589E\u52A0\u4E00\u4E2A\u865A\u6570\u5355\u4F4D `i`\uFF08\u5176\
  \u4E2D `i^2 = -1`\uFF09\uFF0C\u590D\u6570\u6269\u5C55\u4E86\u5B9E\u6570\u7EBF\u3002\
  \u5728\u5DE5\u7A0B\u5B66\u3001\u7269\u7406\u5B66\u4EE5\u53CA\u9AD8\u7EA7\u6570\u5B66\
  \u7B49\u9886\u57DF\uFF0C\u590D\u6570\u81F3\u5173\u91CD\u8981\uFF0C\u56E0\u4E3A\u5B83\
  \u4EEC\u80FD\u591F\u6A21\u62DF\u5B9E\u6570\u65E0\u6CD5\u5904\u7406\u7684\u73B0\u8C61\
  \uFF0C\u5982\u7535\u6D41\u548C\u4FE1\u53F7\u5904\u7406\u3002."
title: "\u5904\u7406\u590D\u6570"
weight: 14
---

## 什么 & 为什么？

通过增加一个虚数单位 `i`（其中 `i^2 = -1`），复数扩展了实数线。在工程学、物理学以及高级数学等领域，复数至关重要，因为它们能够模拟实数无法处理的现象，如电流和信号处理。

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

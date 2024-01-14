---
title:                "C#: 生成随机数"
programming_language: "C#"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 为什么要生成随机数？

在编程中，我们经常需要使用随机数来模拟真实世界的情况或者进行测试。通过生成随机数，我们可以让程序具备一定的随机性，从而提高其可靠性和准确性。

## 如何生成随机数？

在C#中，可以使用内置的Random类来生成随机数。首先，我们需要在代码中引入Random类的命名空间。

```C#
using System;
```

接下来，我们可以通过以下代码创建一个Random对象，并设置种子值：

```C#
Random random = new Random(seed);
```

其中，种子值是一个可选的参数，用于初始化生成随机数的算法。如果不提供种子值，则默认使用当前时间作为种子值。接着，我们可以使用Random对象的Next方法来生成随机整数：

```C#
int randomNumber = random.Next();
```

如果希望生成指定范围内的随机数，可以使用Next方法的重载形式：

```C#
int randomNumberInRange = random.Next(minValue, maxValue);
```

除了整数，Random类还可以生成其他类型的随机数，如随机双精度浮点数：

```C#
double randomDouble = random.NextDouble();
```

## 深入了解随机数生成

随机数的生成实际上是一个伪随机的过程，因为计算机无法产生真正的随机数，只能通过算法来模拟随机性。Random类使用的是线性同余法来生成随机数，它根据当前的种子值和一组固定的数学公式来计算下一个随机数。

另外，需要注意的是，同一个种子值会产生相同的随机数序列。因此，在使用Random类生成随机数时，建议选择一个不会重复的种子值，如当前时刻的毫秒数。

## 参考链接

- [MSDN官方文档](https://docs.microsoft.com/zh-cn/dotnet/api/system.random?view=netframework-4.8)
- [C#随机数生成器的实现原理](https://www.cnblogs.com/jinchun/p/3554466.html)
- [如何生成不重复的随机数](https://www.cnblogs.com/devshaw/archive/2009/04/06/1439853.html)

# 查看相关资料

- [学习C#编程的最佳途径](https://www.cnblogs.com/zqifa/p/csharp-learning-guide.html)
- [随机数生成算法的选择](https://blog.csdn.net/kao_yong/article/details/84965028)
- [在C#中使用LINQ来生成随机数序列](https://www.cnblogs.com/freshman0219/p/6662988.html)
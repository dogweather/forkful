---
title:                "C#: 生成随机数"
simple_title:         "生成随机数"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

为什么：随机数生成是编程中常用的技术之一，它可以帮助我们在需要随机选择或随机生成数据的时候提供便利。例如，在游戏开发中可以用随机数来生成怪物的属性或随机事件的发生概率。

## 如何进行随机数生成

在C#中，我们可以使用Random类来实现随机数的生成。首先，我们需要在程序中引入该类：

```C#
using System;

// 声明一个Random对象
Random rnd = new Random();
```

接着，我们可以使用Random类中的方法来生成不同类型的随机数：

- 生成一个整数：

```C#
int num = rnd.Next(); // 生成一个0到Int32.MaxValue之间的随机整数
```

如果想要限制生成的随机整数的范围，可以在Next()方法中指定最小值和最大值：

```C#
int num = rnd.Next(1, 100); // 生成一个1到99之间的随机整数
```

- 生成一个双精度浮点数：

```C#
double num = rnd.NextDouble(); // 生成一个0到1之间的随机双精度浮点数
```

- 生成一个指定长度的随机字节数组：

```C#
// 指定数组长度为10
byte[] bytes = new byte[10];
rnd.NextBytes(bytes); // 生成10个随机字节
```

- 生成一个布尔值：

```C#
bool flag = rnd.Next(2) == 0; // 生成一个随机布尔值，50%的概率为true
```

生成随机数的方式有很多种，可以根据自己的需求来选择合适的方法。

## 深入随机数生成

在计算机中，随机数的生成实际上并不是完全的随机，而是利用某种算法来产生看似随机的数字序列。因此，我们可以通过种子值来控制随机数的生成结果。

在Random类中，构造函数可以接受一个种子值作为参数，如果不指定种子值，则默认使用系统时间作为种子。

```C#
// 设置种子值为5
Random rnd = new Random(5);
```

同样的种子值会生成同样的随机数序列，这可以用来调试程序。

此外，Random类中还有一个NextBytes()方法，可以生成高质量的随机字节数组。这对于加密和安全性要求较高的程序很有用。

## 参考资料

- [Microsoft Docs - Random Class (System) (C#)](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-5.0)
- [C#中的随机数生成 (Random类)](https://www.runoob.com/w3cnote/csharp-random-programming.html)
- [What Is a Random Number Generator?](https://www.explainthatstuff.com/random-numbers.html)

## 参见

- [C# 中的几种排序算法](https://www.ituring.com.cn/article/4)
- [C# 中的字符串格式化](https://docs.microsoft.com/en-us/dotnet/standard/base-types/composite-formatting)
---
title:                "产生随机数"
html_title:           "C#: 产生随机数"
simple_title:         "产生随机数"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 这是什么 & 为什么要这么做？

生成随机数是指在程序中使用代码来产生一个随机的数字。程序员们通常会使用这种方法来增加程序的灵活性和变化性，使其更加有趣和具有挑战性。

## 如何实现：

### 生成随机整数：

```C#
Random random = new Random(); //创建一个随机对象
int randomNum = random.Next(1, 10); //生成介于1到10之间的随机整数
Console.WriteLine($"随机整数：{randomNum}"); //打印输出结果
```

输出结果可能是：随机整数：7

### 生成随机小数：

```C#
Random random = new Random(); 
double randomNum = random.NextDouble(); //生成介于0到1之间的随机小数
Console.WriteLine($"随机小数：{randomNum}");
```

输出结果可能是：随机小数：0.526849463272571

## 深入探讨：

### 历史背景：

生成随机数在计算机科学领域已经存在很长时间了。早期的计算机并没有内置随机数生成的功能，程序员们不得不自己编写代码来实现这一功能。

### 替代方法：

除了使用Random类来实现随机数生成外，还可以使用Guid类或者CryptographicRandom类来获得更加随机和安全的随机数。

### 实现细节：

在C#中，Random类是基于伪随机算法实现的，即程序员可以通过指定种子值来控制生成的随机数，从而达到重复生成相同序列的目的。

## 参考资料：

- [Microsoft Docs上的Random类文档](https://docs.microsoft.com/zh-cn/dotnet/api/system.random)
- [C#教程中关于随机数生成的部分](https://www.w3schools.com/cs/cs_random.asp)
- [详细介绍CryptographicRandom类的一篇文章](https://www.codeproject.com/Tips/1212069/Csharp-Secure-Random-Numbers-using-CryptoServiceProvider)

整理者：[你的名字]
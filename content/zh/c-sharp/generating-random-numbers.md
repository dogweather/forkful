---
title:    "C#: 生成随机数"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 为什么要生成随机数

在编程过程中，有时我们需要生成随机的数字来模拟现实世界的情况，或者用作密码或加密等安全目的。生成随机数的能力可以为我们的程序增加更多的功能和保证安全性。接下来，我们将学习如何在C#中生成随机数，并深入了解其背后的工作原理。

# 如何生成随机数

在C#中，我们可以使用Random类来生成随机数。首先，我们需要在程序中导入System命名空间，代码如下所示：
```C#
using System;
```
接下来，我们需要创建一个Random的实例，代码如下所示：
```C#
Random rand = new Random();
```
现在，我们可以使用此实例的Next()方法来生成随机整数，代码如下所示：
```C#
int randomInt = rand.Next();
```
我们还可以指定生成的随机数的范围，例如：生成一个1-10之间的随机数，代码如下所示：
```C#
int randomInt = rand.Next(1, 11);
```
此外，我们还可以生成随机小数，代码如下所示：
```C#
double randomDouble = rand.NextDouble();
```

# 深入了解生成随机数

在计算机中，生成随机数并不是一件容易的事情。因为计算机是按照特定的算法运行的，所以它们不能自己产生随机数。相反，它们可以使用特定的算法来生成看似随机的数字。Random类中使用的算法称为“线性同余法”，它使用一个称为“种子”的数字来计算下一个随机数。如果使用相同的种子，将会生成相同的随机数序列。

为了避免这种情况，我们可以使用系统时间作为种子，因为它每次都是不同的。此外，Random类还提供了一个种子字段，我们可以手动设置不同的种子来生成不同的随机数序列。

# 另请参阅

- [Random Class (C# Programming Guide)](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-5.0)
- [How to Generate Random Numbers in C#](https://www.c-sharpcorner.com/UploadFile/cd7c2e/random-numbers-in-C-Sharp/)
- [Understanding the Random Class in C#](https://www.codeproject.com/Tips/1110346/Understanding-the-Random-Class-in-Csharp)
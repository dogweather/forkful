---
title:                "生成随机数"
html_title:           "C#: 生成随机数"
simple_title:         "生成随机数"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 为什么要使用随机数

随机数在编程中非常有用，因为它们可以帮助我们模拟现实世界中的随机事件，例如抽奖、游戏等。使用随机数可以让我们的程序更加实用和有趣。

## 如何生成随机数

在C#中生成随机数非常简单，我们只需要使用 `Random` 类。以下是一个示例代码：

```c#
Random random = new Random(); // 创建Random对象
int randomNumber = random.Next(1, 10); // 生成1到10之间的随机整数
Console.WriteLine("随机数：" + randomNumber); // 输出随机数
```

运行结果可能会类似于 `随机数：7`，每次运行都会产生一个不同的随机数。

我们也可以生成随机小数，例如：

```c#
double randomDouble = random.NextDouble(); // 生成0到1之间的随机小数
Console.WriteLine("随机小数：" + randomDouble); // 输出随机小数
```

运行结果可能类似于 `随机小数：0.693482`。

## 深入了解随机数

在计算机中，随机数实际上是由伪随机数生成器生成的。这些伪随机数是通过数学算法产生的，所以它们并不是真正的随机数，但是它们的表现非常类似。

我们也可以通过设置种子值来控制伪随机数的生成。种子值是伪随机数生成算法的起始点，如果两次使用相同的种子值，就会得到相同的随机数序列。

为了生成更随机的随机数，我们可以使用时间戳作为种子值，例如：

```c#
int timestamp = (int)(DateTime.UtcNow - new DateTime(1970, 1, 1)).TotalSeconds; // 获取当前的时间戳
Random random = new Random(timestamp); // 使用时间戳作为种子值
```

这样每次运行程序都会生成不同的随机数序列。

# 参考链接

- [MSDN: Random Class](https://msdn.microsoft.com/en-us/library/system.random(v=vs.110).aspx)
- [C#中的随机数生成](https://www.cnblogs.com/mzp-t/p/3914499.html)
- [伪随机数生成器](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)

# 参见

- [C#编程指南](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/)
- [随机数生成器简介（维基百科）](https://zh.wikipedia.org/zh-cn/%E4%BC%AA%E9%9A%8F%E6%9C%BA%E6%95%B0%E7%94%9F%E6%88%90%E5%99%A8)
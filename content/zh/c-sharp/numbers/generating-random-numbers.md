---
title:                "生成随机数"
aliases:
- zh/c-sharp/generating-random-numbers.md
date:                  2024-01-27T20:33:08.078113-07:00
model:                 gpt-4-0125-preview
simple_title:         "生成随机数"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么与为什么？

在 C# 中生成随机数涉及在指定范围内创建不可预测的数值。程序员使用这些方法来实现诸如加密、模拟和游戏等特性，其中需要不可预测性或模拟现实世界的随机性。

## 如何操作：

生成 C# 中随机数最常见的方法是使用 `System.Random` 类。这里有一个简单的示例演示了它的用法：

```C#
using System;

public class RandomNumberExample
{
    static void Main(string[] args)
    {
        Random random = new Random();
        int randomNumber = random.Next(1, 100); // 生成一个 1 到 99 之间的数字
        Console.WriteLine($"随机数: {randomNumber}");
    }
}
```

这将输出一个随机数，例如：

```
随机数: 42
```

要生成一个 0.0 到 1.0 之间的随机浮点数，你可以使用 `NextDouble` 方法：

```C#
double randomDouble = random.NextDouble();
Console.WriteLine($"随机双精度: {randomDouble}");
```

如果你正在研发一个对安全性要求高的应用程序，需要加密级的随机性，最好使用 `System.Security.Cryptography` 中的 `RNGCryptoServiceProvider` 类：

```C#
using System;
using System.Security.Cryptography;

public class SecureRandomExample
{
    static void Main()
    {
        byte[] randomNumber = new byte[4]; // 创建一个 4 字节长的随机数
        using (RNGCryptoServiceProvider rng = new RNGCryptoServiceProvider())
        {
            rng.GetBytes(randomNumber);
        }
        int value = BitConverter.ToInt32(randomNumber, 0);
        Console.WriteLine($"加密级安全随机数: {value}");
    }
}
```

## 深入探索

多年来，C# 中的随机数生成已经发展。最初，`System.Random` 类是生成伪随机数的首选。之所以称之为伪随机，是因为给定一个特定的种子值，它将产生相同的数字序列，这对于调试或测试的可重复性是有用的。

虽然对于基本需求来说足够了，`System.Random` 不是线程安全的，并且能产生可预测的结果，这对于依赖安全的应用程序来说是不适合的。这种限制导致了引入 `RNGCryptoServiceProvider` 以获得加密级的随机性，这更安全但也更消耗资源。

在 .NET Core 和 .NET 5+ 中的另一个选择是 `System.Security.Cryptography` 中的 `RandomNumberGenerator` 类，用于安全地生成随机数，旨在提供一个比 `RNGCryptoServiceProvider` 更现代化且易于使用的选项。

C# 中生成随机数的每种方法都有其适用的场合，取决于应用程序的要求。对于大多数应用程序，`System.Random` 就足够了，但对于那些需要安全、不可预测的随机数的应用程序，加密类提供了一个强大的替代方案。

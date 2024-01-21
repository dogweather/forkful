---
title:                "生成随机数"
date:                  2024-01-20T17:49:05.316949-07:00
model:                 gpt-4-1106-preview
simple_title:         "生成随机数"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么和为什么？
随机数生成是创建不可预测数字的程序行为。程序员需要随机数进行测试、游戏逻辑或模拟现实世界场景。

## 怎么做：
在C#中，你可用 `Random` 类或者 .NET 6之后推出的 `RandomNumberGenerator` 类。先来看看 `Random`。

```C#
using System;

class RandomExample
{
    static void Main()
    {
        Random rnd = new Random();
        Console.WriteLine(rnd.Next());     // 生成一个随机整数
        Console.WriteLine(rnd.Next(100));  // 生成0到99的随机数
        Console.WriteLine(rnd.Next(50, 101)); // 生成50到100的随机数
    }
}
```

现在，我们使用 `RandomNumberGenerator.Create()` 方法生成一个安全的随机数：

```C#
using System;
using System.Security.Cryptography;

class RandomNumberGeneratorExample
{
    static void Main()
    {
        byte[] randomNumber = new byte[4];
        using (var rng = RandomNumberGenerator.Create())
        {
            rng.GetBytes(randomNumber);
            int value = BitConverter.ToInt32(randomNumber, 0);
            Console.WriteLine(value); // 输出安全随机数
        }
    }
}
```

## 深入了解：
生成随机数在历史上一直是计算机科学的核心议题之一。传统 `Random` 类基于伪随机数生成器（PRNG），适合大多数日常用途，但它并不适合密码学。相较之下， `RandomNumberGenerator` 类用的是加密安全伪随机数生成器（CSPRNG），更为安全但也较慢。

其他选择包括用第三方库如 `MathNet.Numerics` 或系统特有的方法（如Linux的 `/dev/random`）。

实现细节值得注意的有 `Random` 类会根据系统时钟生成种子，可能导致预测性问题。最佳实践是创建一个静态或相对全局 `Random` 实例，而不是在需要随机数时重新实例化。

## 参考链接：
- [Random Class (System) | Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=netcore-3.1)
- [RandomNumberGenerator Class (System.Security.Cryptography) | Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.randomnumbergenerator?view=netcore-3.1)
- [Cryptographic randomness - Wikipedia](https://en.wikipedia.org/wiki/Cryptographically_secure_pseudorandom_number_generator)
- [How to generate cryptography-secure random numbers | StackExchange Security](https://security.stackexchange.com/questions/3936/how-to-generate-cryptographically-strong-sequences-of-random-numbers)
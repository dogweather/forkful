---
title:                "生成随机数"
html_title:           "Go: 生成随机数"
simple_title:         "生成随机数"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？(What & Why?)

生成随机数是通过编程创建任意数字的过程。程序员通常这样做来模拟不确定性，或给用户提供难以预测的结果。

## 如何实现：(How to:)

在C#中，你可以使用System的Random类来生成随机数。以下代码演示了如何实现生成1到100之间的随机整数。

```C#
using System;

public class Program {
    public static void Main() {
        Random rnd = new Random();
        int randomNumber = rnd.Next(1, 101);
        Console.WriteLine(randomNumber);
    }
}
```
运行这段代码的输出将是1到100之间的任何数字，这取决于生成随机数的瞬间。

## 深入理解(Deep Dive)

在早期的计算机科学中，生成真正的随机数是相当困难的，因为计算机是建立在可预测和一致性的基础上的。因此，我们通常使用伪随机数，而非真随机数。上述代码示例就是这样：每次程序启动都会生成相同的数列，除非你提供不同的种子值。

C#中的Random类使用线性同余生成器（LCG）算法。但该算法并非最佳选择，尤其在需要高随机性应用中。有许多随机数生成器的替代方案如Mersenne Twister。

在实践中，因为Random类不是线程安全的，所以在高度并行的程序中，你可能会需要在每个线程中创建新的Random实例。

## 另请参阅(See Also)

如需更多信息，请参见以下文档：

- [MSDN: Random Class](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=netframework-4.8)
- [Understanding Random Number Generators, and Their Limitations](https://www.codeproject.com/articles/43408/understanding-random-number-generators-and-their-l)

以上就是在C#中生成随机数的入门知识。最重要的是实践和在各种应用程序中尝试和改进。
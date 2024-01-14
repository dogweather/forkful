---
title:    "C#: 将字符串转换为小写"
keywords: ["C#"]
---

{{< edit_this_page >}}

## 为什么

当我们需要处理用户输入时，经常会遇到将字符串转换为小写的情况。这是因为用户可能会输入大写或者混合大小写的字符串，为了提升用户体验，我们需要将其统一转换为小写。

## 如何做

这里给出两种简单的方法来将字符串转换为小写。

```C#
// 方法一：使用ToLower()方法
string str = "Hello, World!";
string lowerStr = str.ToLower();
Console.WriteLine(lowerStr);
```

输出为：
```
hello, world!
```

```C#
// 方法二：使用ToLowerInvariant()方法
string str = "Hello, World!";
string lowerStr = str.ToLowerInvariant();
Console.WriteLine(lowerStr);
```

输出为：
```
hello, world!
```

## 深入探讨

虽然看起来两种方法都很简单，但是它们的实现原理却有所不同。ToLower()方法使用的是当前系统的语言规则来转换字符串，因此在不同的语言环境下，输出可能会有所不同。而ToLowerInvariant()方法则是使用不变的语言规则来转换字符串，因此无论在什么语言环境下，输出都是一致的。

另外，有些小伙伴可能会注意到，在以上例子中，我们使用了ToLower()方法来将字符串转换为小写，那是否也有相应的ToUpper()方法来将字符串转换为大写呢？答案是肯定的，C#中提供了ToLower()和ToUpper()两个方法来分别实现大小写的转换。

## 参考资料

- [Microsoft Docs - String.ToLower Method (System)](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower)
- [Microsoft Docs - String.ToLowerInvariant Method (System)](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolowerinvariant)

## 参见

- [C#字符串处理教程](https://www.bilibili.com/video/BV1jJ411L7t3)
- [C#字符串转换实战](https://www.bilibili.com/video/BV1kA411T7Xy)
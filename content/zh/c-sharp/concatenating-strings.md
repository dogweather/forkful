---
title:    "C#: 连接字符串"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

# 为什么要使用字符串拼接？

在编程中，有时候我们需要将多个字符串拼接在一起，以创建一个新的字符串。这在处理文本信息和构建动态输出时非常有用。

# 如何实现字符串拼接？

在C#中，我们可以使用 "+" 运算符来连接两个字符串，也可以使用String.Concat()方法来连接多个字符串。下面是一个简单的示例，展示如何使用这两种方法来拼接字符串。

```C#
// 使用 "+" 运算符连接字符串
string greeting = "Hello";
string name = "World";
string message = greeting + ", " + name + "!";
Console.WriteLine(message);

// 使用String.Concat()方法连接多个字符串
string fullName = String.Concat("John", " ", "Smith");
Console.WriteLine(fullName);
```

输出:

```
Hello, World!
John Smith
```

# 深入理解字符串拼接

在C#中，字符串是不可变的，因此每次拼接字符串时，都会创建一个新的字符串对象。这可能会影响性能，特别是当需要拼接大量字符串时。

为了解决这个问题，可以使用StringBuilder类来构建可变字符串。它会在原有的字符串上进行操作，从而避免创建大量新的字符串对象，提高了性能。

例如：

```C#
// 使用StringBuilder拼接字符串
StringBuilder sb = new StringBuilder();
sb.Append("Hello");
sb.Append(", ");
sb.Append("World");
sb.Append("!");

string message = sb.ToString();
Console.WriteLine(message);
```

输出:

```
Hello, World!
```

如果需要拼接多个字符串，使用StringBuilder类会比使用"+"运算符或String.Concat()方法更有效率。

# 参考链接

- [C# 字符串拼接](https://docs.microsoft.com/zh-cn/dotnet/csharp/how-to/concatenate-multiple-strings)
- [StringBuilder 类](https://docs.microsoft.com/zh-cn/dotnet/api/system.text.stringbuilder)